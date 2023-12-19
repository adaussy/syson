 /*******************************************************************************
 * Copyright (c) 2023, 2024 Obeo.
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
package org.eclipse.syson.sysml.impl;

import java.util.List;
import java.util.Optional;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.util.EcoreEList;
import org.eclipse.syson.sysml.FeatureMembership;
import org.eclipse.syson.sysml.FeatureTyping;
import org.eclipse.syson.sysml.PartDefinition;
import org.eclipse.syson.sysml.Redefinition;
import org.eclipse.syson.sysml.Subclassification;
import org.eclipse.syson.sysml.SysmlPackage;
import org.eclipse.syson.sysml.TextualRepresentation;
import org.eclipse.syson.sysml.helper.PrettyPrinter;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Part Definition</b></em>'.
 * <!-- end-user-doc -->
 *
 * @generated
 */
public class PartDefinitionImpl extends ItemDefinitionImpl implements PartDefinition {
    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected PartDefinitionImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return SysmlPackage.eINSTANCE.getPartDefinition();
    }


    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated NOT
     */
    @Override
    public EList<TextualRepresentation> getTextualRepresentation() {
        TextualRepresentationImpl repr = new TextualRepresentationImpl();
        repr.setLanguage("fr");
        StringBuilder builder = new StringBuilder();
        if(isIsAbstract()){
            builder.append("abstract ");
        }
        builder.append("part def ");
        builder.append(PrettyPrinter.prettyPrintName(getDeclaredName()));

        // Subclassification
        Optional<Subclassification> subclassification = getOwnedRelationship().stream().filter(t -> SysmlPackage.eINSTANCE.getSubclassification().isSuperTypeOf(t.eClass())).map(t -> (Subclassification) t).findFirst();
        if(subclassification.isPresent()){
            builder.append(":> ");
            if(  subclassification.get().getSuperclassifier() != null ){
                builder.append(PrettyPrinter.prettyPrintName(subclassification.get().getSuperclassifier().getQualifiedName()));
            } else {
                builder.append(PrettyPrinter.prettyPrintName(subclassification.get().getQualifiedName()));
            }
        }

        List<FeatureMembership> featureMemberships = getOwnedFeatureMembership().stream().filter(t -> SysmlPackage.eINSTANCE.getFeatureMembership().isSuperTypeOf(t.eClass())).map(t -> (FeatureMembership) t).toList();

        if(! featureMemberships.isEmpty()){
            builder.append("{");
            for(FeatureMembership featureMembership: featureMemberships){
                for(var textualRepr: featureMembership.getTextualRepresentation()){
                    builder.append("\n");
                    builder.append(textualRepr.getBody());
                }
            }
            builder.append("\n}");
        } else {
            builder.append(";");
        }
    
        repr.setBody(builder.toString());
        List<TextualRepresentation> textualRepresentation = List.of(repr);
        return new EcoreEList.UnmodifiableEList<>(this, SysmlPackage.eINSTANCE.getElement_TextualRepresentation(), textualRepresentation.size(), textualRepresentation.toArray());
    }

} //PartDefinitionImpl
