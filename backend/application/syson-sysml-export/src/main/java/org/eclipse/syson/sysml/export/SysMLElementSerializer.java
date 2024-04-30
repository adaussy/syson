/*******************************************************************************
 * Copyright (c) 2024 Obeo.
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     Obeo - initial API and implementation
 *******************************************************************************/
package org.eclipse.syson.sysml.export;

import static com.google.common.base.Strings.isNullOrEmpty;
import static com.google.common.base.Strings.nullToEmpty;
//import static org.eclipse.syson.sysml.export.utils.StringUtils.toPrintableName;
import static java.util.stream.Collectors.joining;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.syson.sysml.AttributeDefinition;
import org.eclipse.syson.sysml.AttributeUsage;
import org.eclipse.syson.sysml.Comment;
import org.eclipse.syson.sysml.ConjugatedPortDefinition;
import org.eclipse.syson.sysml.ConjugatedPortTyping;
import org.eclipse.syson.sysml.Definition;
import org.eclipse.syson.sysml.Element;
import org.eclipse.syson.sysml.Feature;
import org.eclipse.syson.sysml.FeatureReferenceExpression;
import org.eclipse.syson.sysml.FeatureTyping;
import org.eclipse.syson.sysml.Import;
import org.eclipse.syson.sysml.InterfaceDefinition;
import org.eclipse.syson.sysml.ItemDefinition;
import org.eclipse.syson.sysml.LiteralBoolean;
import org.eclipse.syson.sysml.LiteralExpression;
import org.eclipse.syson.sysml.LiteralInfinity;
import org.eclipse.syson.sysml.LiteralInteger;
import org.eclipse.syson.sysml.LiteralRational;
import org.eclipse.syson.sysml.LiteralString;
import org.eclipse.syson.sysml.Membership;
import org.eclipse.syson.sysml.MembershipImport;
import org.eclipse.syson.sysml.Metaclass;
import org.eclipse.syson.sysml.MetadataUsage;
import org.eclipse.syson.sysml.MultiplicityRange;
import org.eclipse.syson.sysml.Namespace;
import org.eclipse.syson.sysml.NamespaceImport;
import org.eclipse.syson.sysml.OccurrenceDefinition;
import org.eclipse.syson.sysml.OwningMembership;
import org.eclipse.syson.sysml.PartDefinition;
import org.eclipse.syson.sysml.PortDefinition;
import org.eclipse.syson.sysml.Redefinition;
import org.eclipse.syson.sysml.ReferenceSubsetting;
import org.eclipse.syson.sysml.Subclassification;
import org.eclipse.syson.sysml.Subsetting;
import org.eclipse.syson.sysml.Usage;
import org.eclipse.syson.sysml.VisibilityKind;
import org.eclipse.syson.sysml.export.utils.Appender;
import org.eclipse.syson.sysml.export.utils.NameDeresolver;
import org.eclipse.syson.sysml.helper.EMFUtils;
import org.eclipse.syson.sysml.helper.LabelConstants;
import org.eclipse.syson.sysml.util.SysmlSwitch;
import org.eclipse.syson.sysml.Package;

import reactor.util.function.Tuples;

/**
 * Convert a SysML {@link Element} to its textual representation.
 * 
 * @author Arthur Daussy
 */
public class SysMLElementSerializer extends SysmlSwitch<String> {

    private static final Predicate<Object> NOT_NULL = s -> s != null;

    private String lineSeparator;

    private String indentation;

    private NameDeresolver nameDeresolver;

    /**
     * Simple constructor.
     * 
     * @param newLine
     *            the string used to separate line
     * @param indentation
     *            the string used to indent the file
     */
    public SysMLElementSerializer(String lineSeparator, String indentation, NameDeresolver nameDeresolver) {
        super();
        this.lineSeparator = lineSeparator;
        this.indentation = indentation;
        this.nameDeresolver = nameDeresolver;
    }

    public SysMLElementSerializer() {
        this(System.lineSeparator(), "\t", new NameDeresolver());
    }

    @Override
    public String doSwitch(EObject eObject) {
        String value = super.doSwitch(eObject);
        if (value != null && value.trim().isBlank()) {
            return null;
        } else {
            return value;
        }
    }

    @Override
    public String casePackage(Package pack) {

        Appender builder = this.newAppender();

        builder.append("package ");
        this.appendNameWithShortName(builder, pack);

        this.appendOwnedRelationshiptContent(builder, pack);

        return builder.toString();
    }

    /**
     * This method handle most of the {@link Definition} elements. Some elements may required some custom handling. In
     * this case look for a more specific method.
     */
    @Override
    public String caseDefinition(Definition def) {
        Appender builder = newAppender();

        if (def instanceof OccurrenceDefinition occDef) {
            appendDefinitionPrefix(builder, occDef);
        }

        builder.appendSpaceIfNeeded().append(getKeyword(def));

        appendDefinition(builder, def);

        return builder.toString();
    }

    public String caseLiteralExpression(LiteralExpression expression) {
        Appender builder = this.newAppender();

        if (expression instanceof LiteralInteger lit) {
            builder.append(String.valueOf(lit.getValue()));
        } else if (expression instanceof LiteralString lit) {
            builder.append(lit.getValue());
        } else if (expression instanceof LiteralRational lit) {
            builder.append(String.valueOf(lit.getValue()));
        } else if (expression instanceof LiteralBoolean lit) {
            builder.append(String.valueOf(lit.isValue()));
        } else if (expression instanceof LiteralInfinity lit) {
            builder.append("*");
        }

        return builder.toString();
    }

    @Override
    public String caseAttributeUsage(AttributeUsage attribute) {

        Appender builder = this.newAppender();

        this.appendUsagePrefix(builder, attribute);

        builder.appendSpaceIfNeeded().append("attribute ");

        this.appendUsageDeclaration(builder, attribute);

        appendOwnedRelationshiptContent(builder, attribute);

        return builder.toString();
    }

    private void appendFeatureSpecilizationPart(Appender builder, Feature feature) {
        List<Redefinition> ownedRedefinition = feature.getOwnedRedefinition();
        appendRedefinition(builder, ownedRedefinition, feature);

        appendReferenceSubsetting(builder, feature.getOwnedReferenceSubsetting(), feature);

        List<Subsetting> ownedSubsetting = new ArrayList<>(feature.getOwnedSubsetting());
        ownedSubsetting.removeAll(ownedRedefinition);
        ownedSubsetting.remove(feature.getOwnedReferenceSubsetting());

        appendSubsettings(builder, ownedSubsetting, feature);

        appendFeatureTyping(builder, feature.getOwnedTyping(), feature);
        this.appendMultiplicityPart(builder, feature);
    }

    private void appendSubsettings(Appender builder, List<Subsetting> subSettings, Element element) {
        if (!subSettings.isEmpty()) {
            builder.appendSpaceIfNeeded().append(LabelConstants.SUBSETTING);
            builder.appendSpaceIfNeeded().append(subSettings.stream().map(getSubsettedFeature())
                    .filter(Objects::nonNull)
                    .map(superFeature -> getDeresolvableName(superFeature, element))
                    .collect(Collectors.joining(", ")));
        }

    }

    // TODO : when the function Subsetting::getSubsettedFeature() is correctly implemented then remove this function and
    // use only Subsetting::getSubsettedFeature() in appendSubsettings();
    private Function<? super Subsetting, ? extends Feature> getSubsettedFeature() {
        return subsetting -> {
            Feature subsettedFeature = subsetting.getSubsettedFeature();
            if (subsettedFeature != null) {
                return subsettedFeature;
            } else {
                return subsettedFeature;
            }
        };
    }

    private void appendRedefinition(Appender builder, List<Redefinition> redefinitions, Element element) {
        if (!redefinitions.isEmpty()) {
            builder.appendSpaceIfNeeded().append(LabelConstants.REDEFINES);
            builder.appendSpaceIfNeeded().append(redefinitions.stream().map(Redefinition::getRedefinedFeature)
                    .filter(Objects::nonNull)
                    .map(superFeature -> getDeresolvableName(superFeature, element))
                    .collect(Collectors.joining(", ")));
        }

    }

    private void appendReferenceSubsetting(Appender builder, ReferenceSubsetting ownedReferenceSubsetting, Element element) {
        if (ownedReferenceSubsetting != null) {
            builder.appendSpaceIfNeeded().append(LabelConstants.REFERENCES);
            if (ownedReferenceSubsetting.getReferencedFeature() != null) {
                builder.appendSpaceIfNeeded().append(getDeresolvableName(ownedReferenceSubsetting.getReferencedFeature(), element));
            }
        }

    }

    /**
     * @param builder
     * @param ownedTyping
     * @param element
     */
    private void appendFeatureTyping(Appender builder, EList<FeatureTyping> ownedTyping, Element element) {
        if (!ownedTyping.isEmpty()) {
            builder.appendSpaceIfNeeded().append(LabelConstants.DEFINED_BY);
            builder.appendSpaceIfNeeded().append(ownedTyping.stream()
                    .filter(ft -> ft.getType() != null)
                    .map(ft -> Tuples.of(ft, ft.getType()))
                    .filter(t -> t.getT2() != null)
                    .map(t -> {
                        String name = getDeresolvableName(t.getT2(), element);

                        if (t.getT1() instanceof ConjugatedPortTyping) {
                            name = LabelConstants.CONJUGATED + name;
                        }
                        return name;
                    }

                    )
                    .collect(Collectors.joining(", ")));
        }
    }

    private void appendMultiplicityPart(Appender builder, Feature feature) {
        MultiplicityRange multiplicity = feature.getOwnedElement().stream()
                .filter(MultiplicityRange.class::isInstance)
                .map(MultiplicityRange.class::cast)
                .findFirst()
                .orElse(null);

        if (multiplicity != null) {
            String expression = multiplicity.getOwnedElement().stream()
                    .filter(element -> element instanceof LiteralExpression || element instanceof FeatureReferenceExpression)
                    .map(element -> {
                        String exp = null;
                        if (element instanceof LiteralExpression lit) {
                            exp = caseLiteralExpression(lit);
                        } else if (element instanceof FeatureReferenceExpression featureReferenceExp) {
                            exp = getDeresolvableName(featureReferenceExp, feature);
                        }
                        return exp;
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.joining(".."));

            if (!expression.isEmpty()) {
                builder.appendSpaceIfNeeded().append(LabelConstants.OPEN_BRACKET)
                        .append(expression)
                        .append(LabelConstants.CLOSE_BRACKET);
            }

            boolean isOrdered = multiplicity.isIsOrdered();
            boolean isUnique = multiplicity.isIsUnique();

            if (isOrdered) {
                builder.appendSpaceIfNeeded().append("ordered");
                if (!isUnique) {
                    builder.appendSpaceIfNeeded().append("nonunique");
                }
            }
        }

    }

    @Override
    public String caseConjugatedPortDefinition(ConjugatedPortDefinition object) {
        // Conjugated port definition are implicit
        return "";
    }

    private String getKeyword(Definition def) {
        final String keyword;

        if (def instanceof InterfaceDefinition) {
            keyword = "interface def";
        } else if (def instanceof PartDefinition) {
            keyword = "part def";
        } else if (def instanceof PortDefinition) {
            keyword = "port def";
        } else if (def instanceof AttributeDefinition) {
            keyword = "attribute def";
        } else if (def instanceof ItemDefinition) {
            keyword = "item def";
        } else {
            keyword = null;
        }

        return keyword;
    }

    private void appendDefinition(Appender builder, Definition definition) {

        appendDefinitionDeclaration(builder, definition);

        // definition body
        appendOwnedRelationshiptContent(builder, definition);
    }

    private void appendDefinitionDeclaration(Appender builder, Definition definition) {
        appendNameWithShortName(builder, definition);

        EList<Subclassification> subClassification = definition.getOwnedSubclassification();
        if (!subClassification.isEmpty()) {
            builder.appendSpaceIfNeeded().append(":> ");

            String superClasses = subClassification.stream()
                    .map(sub -> sub.getSuperclassifier())
                    .filter(NOT_NULL)
                    .map(sup -> getDeresolvableName(sup, definition))
                    .collect(joining(", "));

            builder.append(superClasses);

        }
    }

    private void appendUsageDeclaration(Appender builder, Usage usage) {
        appendNameWithShortName(builder, usage);

        appendFeatureSpecilizationPart(builder, usage);
    }

    /**
     * Get a deresolvable name for a given element in a given context
     * 
     * @param toDeresolve
     *            the object to deresolve
     * @param context
     *            a context
     * @return a name
     */
    private String getDeresolvableName(Element toDeresolve, Element context) {
        return nameDeresolver.getDeresolvedName(toDeresolve, context);
    }

    private void appendDefinitionPrefix(Appender builder, Definition def) {

        builder.appendSpaceIfNeeded().append(getBasicDefinitionPrefix(def));

        if (def instanceof OccurrenceDefinition occDef) {

            final String isIndividual;
            if (occDef.isIsIndividual()) {
                isIndividual = "individual";
            } else {
                isIndividual = "";
            }
            builder.appendSpaceIfNeeded().append(isIndividual);
        }

        appendDefinitionExtensionKeyword(builder, def);
    }

    private void appendUsagePrefix(Appender builder, Usage usage) {

        getBasicUsagePrefix(builder, usage);

        final String isRef;
        if (usage.isIsReference()) {
            isRef = "ref";
        } else {
            isRef = "";
        }

        builder.appendSpaceIfNeeded().append(isRef);

        appendUsageExtensionKeyword(builder, usage);
    }

    private void appendUsageExtensionKeyword(Appender builder, Usage usage) {
        for (var rel : usage.getOwnedRelationship()) {
            if (rel instanceof OwningMembership owningMember) {
                owningMember.getOwnedRelatedElement().stream()
                        .filter(MetadataUsage.class::isInstance)
                        .map(MetadataUsage.class::cast)
                        .map(MetadataUsage::getMetadataDefinition)
                        .filter(NOT_NULL)
                        .forEach(mDef -> appendPrefixMetadataMember(builder, mDef));
            }
        }
    }

    private void appendDefinitionExtensionKeyword(Appender builder, Definition def) {
        for (var rel : def.getOwnedRelationship()) {
            if (rel instanceof OwningMembership owningMember) {
                owningMember.getOwnedRelatedElement().stream()
                        .filter(MetadataUsage.class::isInstance)
                        .map(MetadataUsage.class::cast)
                        .map(MetadataUsage::getMetadataDefinition)
                        .filter(NOT_NULL)
                        .forEach(mDef -> appendPrefixMetadataMember(builder, mDef));
            }
        }
    }

    private void appendPrefixMetadataMember(Appender builder, Metaclass def) {
        builder.appendSpaceIfNeeded().append("#");
        appendSimpleName(builder, def);
    }

    private void appendSimpleName(Appender appender, Element e) {
        String shortName = e.getName();
        String declaredName = e.getDeclaredName();
        if (shortName != null && !shortName.isBlank()) {
            appender.appendPrintableName(shortName);
        } else if (declaredName != null && !declaredName.isBlank()) {
            appender.appendPrintableName(declaredName);

        } else {
            appender.appendPrintableName(e.effectiveName());
        }
    }

    private String getBasicDefinitionPrefix(Definition def) {
        StringBuilder builder = new StringBuilder();
        if (def.isIsAbstract()) {
            builder.append("abstract");
        }
        if (def.isIsVariation()) {
            if (!builder.isEmpty()) {
                builder.append(" ");
            }
            builder.append("variation");
        }
        return builder.toString();
    }

    private void getBasicUsagePrefix(Appender builder, Usage usage) {
        if (usage.isIsAbstract()) {
            builder.appendSpaceIfNeeded();
            builder.append("abstract");
        }
        if (usage.isIsVariation()) {
            builder.appendSpaceIfNeeded();
            builder.append("variation");
        }
        if (usage.isIsReadOnly()) {
            builder.appendSpaceIfNeeded();
            builder.append("readonly");
        }
        if (usage.isIsDerived()) {
            builder.appendSpaceIfNeeded();
            builder.append("derived");
        }
        if (usage.isIsEnd()) {
            builder.appendSpaceIfNeeded();
            builder.append("end");
        }
    }

    private Appender newAppender() {
        return new Appender(lineSeparator, indentation);
    }

    private void appendOwnedRelationshiptContent(Appender builder, Element element) {

        String content = this.getContent(element.getOwnedRelationship());
        if (content != null && !content.isBlank()) {
            builder.append(" {");
            builder.appendIndentedContent(content);
            builder.newLine().append("}");
        } else {
            builder.append(";");
        }
    }

    private String getContent(List<? extends Element> children) {
        return children.stream().map(rel -> this.doSwitch(rel)).filter(NOT_NULL).collect(joining(lineSeparator, lineSeparator, ""));
    }

    @Override
    public String caseImport(Import aImport) {

        Appender builder = this.newAppender();

        VisibilityKind visibility = aImport.getVisibility();
        if (visibility != VisibilityKind.PUBLIC) {
            builder.append(this.getVisivilityIndicator(visibility));
        }
        builder.appendSpaceIfNeeded().append("import ");

        if (aImport.isIsImportAll()) {
            builder.appendSpaceIfNeeded().append("all");
        }

        if (aImport instanceof NamespaceImport namespaceImport) {
            this.appendNamespaceImport(builder, namespaceImport);
        } else if (aImport instanceof MembershipImport membershipImport) {
            this.appendMembershipImport(builder, membershipImport);
        }

        if (aImport.isIsRecursive()) {
            builder.append("::**");
        }

        this.appendOwnedRelationshiptContent(builder, aImport);

        return builder.toString();
    }

    @Override
    public String caseComment(Comment comment) {

        Appender builder = this.newAppender();
        EList<Element> annotatedElements = comment.getAnnotatedElement();
        boolean selfNamespaceDescribingComment = this.isSelfNamespaceDescribingComment(comment);
        if (isNullOrEmpty(comment.getLocale()) && selfNamespaceDescribingComment) {
            builder.append(this.getCommentBody(comment.getBody()));
        } else {
            builder.append("comment");

            this.appendNameWithShortName(builder, comment);

            if (!selfNamespaceDescribingComment) {
                this.appendAnnotatedElements(builder, comment, annotatedElements);
            }

            this.appendLocale(builder, comment.getLocale());

            builder.newLine().indent();
            builder.appendIndentedContent(this.getCommentBody(comment.getBody()));
        }

        return builder.toString();
    }

    /**
     * Returns true if the comment describes its <b>direct</b> owning namespace
     * 
     * @param comment
     *            a comment
     * @return true if described is direct owning namespace
     */
    private boolean isSelfNamespaceDescribingComment(Comment comment) {
        EList<Element> annotatedElements = comment.getAnnotatedElement();
        if (!annotatedElements.isEmpty()) {
            Element annotatedElement = annotatedElements.get(0);
            if (annotatedElement instanceof Namespace owningNamespace && (owningNamespace == this.getDirectContainer(comment, Namespace.class))) {
                return true;
            }
        }
        return false;
    }

    private void appendAnnotatedElements(Appender builder, Comment comment, EList<Element> annotatedElements) {
        if (!annotatedElements.isEmpty()) {
            builder.appendSpaceIfNeeded().append("about ");
            builder.append(annotatedElements.stream().map(e -> getDeresolvableName(e, comment)).collect(joining(",")));
        }
    }

    /**
     * Returns the direct container of the element with the expected type. A direct container is either the
     * {@link EObject#eContainer()} or the container of the {@link OwningMembership}
     * 
     * @param element
     *            an element
     * @param expected
     *            the expected type
     * @return the expected type or <code>null</code> if no direct container of the expected type
     */
    private <T> T getDirectContainer(EObject element, Class<T> expected) {
        EObject eContainer = element.eContainer();
        T result = null;
        if (expected.isInstance(eContainer)) {
            result = (T) eContainer;
        } else if (eContainer instanceof OwningMembership owning && expected.isInstance(owning.eContainer())) {
            result = (T) owning.eContainer();
        }
        return result;
    }

    private String getCommentBody(String body) {
        Appender subBuilder = this.newAppender();
        subBuilder.append("/* ").append(body).append(" */");
        return subBuilder.toString();
    }

    private void appendLocale(Appender builder, String local) {
        if (!isNullOrEmpty(local)) {
            builder.appendSpaceIfNeeded().append("locale").append(" \"").append(local).append("\"");
        }
    }

    @Override
    public String caseOwningMembership(OwningMembership owningMembership) {
        Appender builder = this.newAppender();

        VisibilityKind visibility = owningMembership.getVisibility();
        if (visibility != VisibilityKind.PUBLIC) {
            builder.append(this.getVisivilityIndicator(visibility));
        }

        String content = owningMembership.getOwnedRelatedElement().stream().map(rel -> this.doSwitch(rel)).filter(NOT_NULL).collect(joining(builder.getNewLine()));
        builder.appendSpaceIfNeeded().append(content);

        return builder.toString();
    }

    private void appendNamespaceImport(Appender builder, NamespaceImport namespaceImport) {
        Namespace importedNamespace = namespaceImport.getImportedNamespace();
        if (importedNamespace != null) {
            builder.appendSpaceIfNeeded().append(this.buildImportContextRelativeQualifiedName(importedNamespace, namespaceImport)).append("::");
        }
        builder.append("*");
    }

    private void appendMembershipImport(Appender builder, MembershipImport membershipImport) {

        Membership importedMembership = membershipImport.getImportedMembership();
        if (importedMembership != null) {
            String qnName = Stream.concat(Stream.ofNullable(importedMembership.getMemberElement()), importedMembership.getOwnedRelatedElement().stream()).filter(e -> e != null).findFirst()
                    .map(e -> this.buildImportContextRelativeQualifiedName(e, membershipImport)).orElse("");

            builder.appendSpaceIfNeeded().append(qnName);
        }
    }

    private String buildImportContextRelativeQualifiedName(Element element, Element from) {
        String qualifiedName = nullToEmpty(element.getQualifiedName());
        Element commonAncestor = EMFUtils.getLeastCommonContainer(Element.class, element, from);
        if (commonAncestor != null) {
            String prefix = commonAncestor.getQualifiedName() + "::";
            if (qualifiedName.startsWith(prefix)) {
                return qualifiedName.substring(prefix.length(), qualifiedName.length());
            }
        }
        return qualifiedName;
    }

    private String appendNameWithShortName(Appender builder, Element element) {
        String shortName = element.getShortName();
        if (!isNullOrEmpty(shortName)) {
            builder.appendSpaceIfNeeded().append("<").appendPrintableName(shortName).append(">");
        }
        String name = element.getDeclaredName();
        if (!isNullOrEmpty(name)) {
            builder.appendSpaceIfNeeded().appendPrintableName(name);
        }
        return builder.toString();
    }

    public String getVisivilityIndicator(VisibilityKind visibility) {
        if (visibility == null) {
            return "";
        }
        return switch (visibility) {
            case PRIVATE -> "private";
            case PROTECTED -> "protected";
            case PUBLIC -> "public";
            default -> "";
        };
    }
}
