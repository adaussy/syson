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
package org.eclipse.syson.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.eclipse.sirius.components.diagrams.description.NodeDescription;
import org.eclipse.sirius.components.representations.VariableManager;
import org.eclipse.syson.sysml.Element;

/**
 * Services for node descriptions.
 *
 * @author Jerome Gout
 */
public class NodeDescriptionService {

    /**
     * Returns the list of candidate node descriptions that can be used to render the given semantic element knowing its
     * parent.
     *
     * @param element
     *            the semantic element for which the node description is searched
     * @param ownerObject
     *            the parent element of the semantic element
     * @param nodeDescriptions
     *            the list of possible parent node descriptions among which the search is performed
     * @param convertedNodes
     *            the map of all existing node descriptions in the DiagramDescription of this Diagram. It corresponds to
     *            a variable accessible from the variable manager.
     * @return the list of node descriptions that can be used to render the given semantic element.
     */
    public List<NodeDescription> getNodeDescriptionsForRenderingElement(Element element, Object ownerObject, List<NodeDescription> nodeDescriptions,
            Map<org.eclipse.sirius.components.view.diagram.NodeDescription, NodeDescription> convertedNodes) {
        List<NodeDescription> candidates = new ArrayList<>();

        for (NodeDescription node : nodeDescriptions) {
            List<NodeDescription> allChildNodeDescriptions = Stream.concat(
                    node.getChildNodeDescriptions().stream(),
                    convertedNodes.values().stream().filter(convNode -> node.getReusedChildNodeDescriptionIds().contains(convNode.getId())))
                    .toList();
            for (NodeDescription childNodeDescription : allChildNodeDescriptions) {
                if (this.canNodeDescritionRenderElement(childNodeDescription, element, ownerObject)) {
                    candidates.add(node);
                }
            }
        }
        return candidates;
    }

    /**
     * Check whether the node will be used to render 'element'. This is the case if the semanticElementProvider returns
     * 'element' when applied to its parent, and the shouldRenderPredicate (i.e. the node's precondition) returns true.
     *
     * @param nodeDescription
     *            the node description to evaluate
     * @param element
     *            the semantic element to render
     * @param ownerObject
     *            the parent element
     * @return <code>true</code> if the given node description can be used to render the given semantic element and
     *         <code>false</code> otherwise.
     */
    public boolean canNodeDescritionRenderElement(NodeDescription nodeDescription, Element element, Object ownerObject) {
        VariableManager semanticElementsProviderVariableManager = new VariableManager();
        semanticElementsProviderVariableManager.put("self", ownerObject);
        List<?> candidatesList = nodeDescription.getSemanticElementsProvider().apply(semanticElementsProviderVariableManager);

        VariableManager shouldRenderPredicateVariableManager = new VariableManager();
        shouldRenderPredicateVariableManager.put("self", element);
        boolean shouldRender = nodeDescription.getShouldRenderPredicate().test(shouldRenderPredicateVariableManager);

        return candidatesList.contains(element) && shouldRender;
    }
}
