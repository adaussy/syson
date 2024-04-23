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
package org.eclipse.syson.diagram.common.view.nodes;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.sirius.components.view.builder.IViewDiagramElementFinder;
import org.eclipse.sirius.components.view.builder.generated.FreeFormLayoutStrategyDescriptionBuilder;
import org.eclipse.sirius.components.view.builder.generated.NodeToolSectionBuilder;
import org.eclipse.sirius.components.view.builder.providers.IColorProvider;
import org.eclipse.sirius.components.view.diagram.DiagramDescription;
import org.eclipse.sirius.components.view.diagram.DropNodeTool;
import org.eclipse.sirius.components.view.diagram.EdgeTool;
import org.eclipse.sirius.components.view.diagram.InsideLabelDescription;
import org.eclipse.sirius.components.view.diagram.InsideLabelPosition;
import org.eclipse.sirius.components.view.diagram.InsideLabelStyle;
import org.eclipse.sirius.components.view.diagram.NodeContainmentKind;
import org.eclipse.sirius.components.view.diagram.NodeDescription;
import org.eclipse.sirius.components.view.diagram.NodePalette;
import org.eclipse.sirius.components.view.diagram.NodeStyleDescription;
import org.eclipse.sirius.components.view.diagram.NodeTool;
import org.eclipse.sirius.components.view.diagram.NodeToolSection;
import org.eclipse.sirius.components.view.diagram.SynchronizationPolicy;
import org.eclipse.syson.diagram.common.view.services.ViewEdgeToolSwitch;
import org.eclipse.syson.diagram.common.view.tools.ToolSectionDescription;
import org.eclipse.syson.sysml.SysmlPackage;
import org.eclipse.syson.sysmlcustomnodes.SysMLCustomnodesFactory;
import org.eclipse.syson.sysmlcustomnodes.SysMLPackageNodeStyleDescription;
import org.eclipse.syson.util.AQLConstants;
import org.eclipse.syson.util.IDescriptionNameGenerator;
import org.eclipse.syson.util.SysMLMetamodelHelper;
import org.eclipse.syson.util.ViewConstants;

/**
 * Used to create the package node description in all diagrams.
 *
 * @author arichard
 */
public abstract class AbstractPackageNodeDescriptionProvider extends AbstractNodeDescriptionProvider {

    protected final IDescriptionNameGenerator nameGenerator;

    public AbstractPackageNodeDescriptionProvider(IColorProvider colorProvider, IDescriptionNameGenerator nameGenerator) {
        super(colorProvider);
        this.nameGenerator = Objects.requireNonNull(nameGenerator);
    }

    /**
     * Implementers should provide the list of {@link NodeDescription} that is reused as child in that Package node
     * description.
     *
     * @param cache
     *            the cache used to retrieve node descriptions.
     * @return the list of {@link NodeDescription} that are added as reused child.
     */
    protected abstract List<NodeDescription> getReusedChildren(IViewDiagramElementFinder cache);

    /**
     * Implementers should provide the list of {@link NodeDescription} that can be dropped inside this Package
     * {@link NodeDescription}.
     *
     * @param cache
     *            the cache used to retrieve node descriptions.
     * @return the list of {@link NodeDescription} that can be dropped inside this package.
     */
    protected abstract List<NodeDescription> getDroppableNodes(IViewDiagramElementFinder cache);

    /**
     * Implementers should provide the list of all {@link NodeDescription} defined in the diagram.<br>
     * This list is used to create edge tools associated to this package.
     *
     * @param cache
     *            the cache used to retrieve node descriptions.
     * @return the list of all {@link NodeDescription} defined in the diagram.
     */
    protected abstract List<NodeDescription> getAllNodeDescriptions(IViewDiagramElementFinder cache);

    /**
     * Implementers should provide the list of tool section descriptions used inside this {@link NodeDescription}.
     *
     * @return the list of tool section descriptions.
     */
    protected abstract List<ToolSectionDescription> getToolSections();

    @Override
    public NodeDescription create() {
        String domainType = SysMLMetamodelHelper.buildQualifiedName(SysmlPackage.eINSTANCE.getPackage());
        return this.diagramBuilderHelper.newNodeDescription()
                .collapsible(true)
                .childrenLayoutStrategy(new FreeFormLayoutStrategyDescriptionBuilder().build())
                .defaultHeightExpression("300")
                .defaultWidthExpression("300")
                .domainType(domainType)
                .insideLabel(this.createInsideLabelDescription())
                .name(this.nameGenerator.getNodeName(SysmlPackage.eINSTANCE.getPackage()))
                .semanticCandidatesExpression("aql:self.getAllReachable(" + domainType + ")")
                .style(this.createPackageNodeStyle())
                .userResizable(true)
                .synchronizationPolicy(SynchronizationPolicy.UNSYNCHRONIZED)
                .build();
    }

    @Override
    public void link(DiagramDescription diagramDescription, IViewDiagramElementFinder cache) {
        var optPackageNodeDescription = cache.getNodeDescription(this.nameGenerator.getNodeName(SysmlPackage.eINSTANCE.getPackage()));
        NodeDescription packageNodeDescription = optPackageNodeDescription.get();
        diagramDescription.getNodeDescriptions().add(packageNodeDescription);

        packageNodeDescription.getReusedChildNodeDescriptions().addAll(this.getReusedChildren(cache));

        packageNodeDescription.setPalette(this.createNodePalette(packageNodeDescription, cache));
    }

    protected InsideLabelDescription createInsideLabelDescription() {
        return this.diagramBuilderHelper.newInsideLabelDescription()
                .labelExpression("aql:self.getContainerLabel()")
                .position(InsideLabelPosition.TOP_CENTER)
                .style(this.createInsideLabelStyle())
                .build();
    }

    protected InsideLabelStyle createInsideLabelStyle() {
        return this.diagramBuilderHelper.newInsideLabelStyle()
                .displayHeaderSeparator(false)
                .labelColor(this.colorProvider.getColor(ViewConstants.DEFAULT_LABEL_COLOR))
                .showIcon(true)
                .withHeader(false)
                .build();
    }

    protected NodeStyleDescription createPackageNodeStyle() {
        SysMLPackageNodeStyleDescription nodeStyleDescription = SysMLCustomnodesFactory.eINSTANCE.createSysMLPackageNodeStyleDescription();
        nodeStyleDescription.setBorderColor(this.colorProvider.getColor(ViewConstants.DEFAULT_BORDER_COLOR));
        nodeStyleDescription.setBorderRadius(0);
        nodeStyleDescription.setColor(this.colorProvider.getColor(ViewConstants.DEFAULT_BACKGROUND_COLOR));
        return nodeStyleDescription;
    }

    private NodePalette createNodePalette(NodeDescription nodeDescription, IViewDiagramElementFinder cache) {
        var changeContext = this.viewBuilderHelper.newChangeContext()
                .expression("aql:self.deleteFromModel()");

        var deleteTool = this.diagramBuilderHelper.newDeleteTool()
                .name("Delete from Model")
                .body(changeContext.build());

        var callEditService = this.viewBuilderHelper.newChangeContext()
                .expression(AQLConstants.AQL_SELF + ".directEdit(newLabel)");

        var editTool = this.diagramBuilderHelper.newLabelEditTool()
                .name("Edit")
                .initialDirectEditLabelExpression(AQLConstants.AQL_SELF + ".getDefaultInitialDirectEditLabel()")
                .body(callEditService.build());

        var edgeTools = new ArrayList<EdgeTool>();
        edgeTools.addAll(this.getEdgeTools(nodeDescription, cache));

        return this.diagramBuilderHelper.newNodePalette()
                .deleteTool(deleteTool.build())
                .labelEditTool(editTool.build())
                .dropNodeTool(this.createDropFromDiagramTool(cache))
                .toolSections(this.createToolSections(cache))
                .edgeTools(edgeTools.toArray(EdgeTool[]::new))
                .build();
    }

    private List<EdgeTool> getEdgeTools(NodeDescription nodeDescription, IViewDiagramElementFinder cache) {
        ViewEdgeToolSwitch edgeToolSwitch = new ViewEdgeToolSwitch(nodeDescription, this.getAllNodeDescriptions(cache), this.nameGenerator);
        return edgeToolSwitch.doSwitch(SysmlPackage.eINSTANCE.getPackage());
    }

    private DropNodeTool createDropFromDiagramTool(IViewDiagramElementFinder cache) {
        var dropElementFromDiagram = this.viewBuilderHelper.newChangeContext()
                .expression("aql:droppedElement.dropElementFromDiagram(droppedNode, targetElement, targetNode, editingContext, diagramContext, convertedNodes)");

        return this.diagramBuilderHelper.newDropNodeTool()
                .name("Drop from Diagram")
                .acceptedNodeTypes(this.getDroppableNodes(cache).toArray(NodeDescription[]::new))
                .body(dropElementFromDiagram.build())
                .build();
    }

    private NodeTool createNodeTool(NodeDescription nodeDescription, EClass eClass) {
        var changeContextNewInstance = this.viewBuilderHelper.newChangeContext()
                .expression("aql:newInstance.elementInitializer()");

        var createEClassInstance = this.viewBuilderHelper.newCreateInstance()
                .typeName(SysMLMetamodelHelper.buildQualifiedName(eClass))
                .referenceName(SysmlPackage.eINSTANCE.getRelationship_OwnedRelatedElement().getName())
                .variableName("newInstance")
                .children(changeContextNewInstance.build());

        var createView = this.diagramBuilderHelper.newCreateView()
                .containmentKind(NodeContainmentKind.CHILD_NODE)
                .elementDescription(nodeDescription)
                .parentViewExpression("aql:selectedNode")
                .semanticElementExpression("aql:newInstance")
                .variableName("newInstanceView");

        var changeContexMembership = this.viewBuilderHelper.newChangeContext()
                .expression("aql:newOwningMembership")
                .children(createEClassInstance.build(), createView.build());

        var createMembership = this.viewBuilderHelper.newCreateInstance()
                .typeName(SysMLMetamodelHelper.buildQualifiedName(SysmlPackage.eINSTANCE.getOwningMembership()))
                .referenceName(SysmlPackage.eINSTANCE.getElement_OwnedRelationship().getName())
                .variableName("newOwningMembership")
                .children(changeContexMembership.build());

        return this.diagramBuilderHelper.newNodeTool()
                .name(this.nameGenerator.getCreationToolName(eClass))
                .iconURLsExpression("/icons/full/obj16/" + eClass.getName() + ".svg")
                .body(createMembership.build())
                .build();
    }

    private NodeToolSection[] createToolSections(IViewDiagramElementFinder cache) {
        var sections = new ArrayList<NodeToolSection>();

        this.getToolSections().forEach(sectionTool -> {
            NodeToolSectionBuilder sectionBuilder = this.diagramBuilderHelper.newNodeToolSection()
                    .name(sectionTool.name())
                    .nodeTools(this.createElementsOfToolSection(cache, sectionTool.elements()));
            sections.add(sectionBuilder.build());
        });

        sections.add(this.addElementsToolSection());

        return sections.toArray(NodeToolSection[]::new);
    }

    private NodeTool[] createElementsOfToolSection(IViewDiagramElementFinder cache, List<EClass> elements) {
        var nodeTools = new ArrayList<NodeTool>();

        elements.forEach(definition -> {
            nodeTools.add(this.createNodeTool(cache.getNodeDescription(this.nameGenerator.getNodeName(definition)).get(), definition));
        });

        nodeTools.sort((nt1, nt2) -> nt1.getName().compareTo(nt2.getName()));

        return nodeTools.toArray(NodeTool[]::new);
    }
}
