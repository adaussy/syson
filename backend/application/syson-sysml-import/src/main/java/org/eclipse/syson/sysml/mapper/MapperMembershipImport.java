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
package org.eclipse.syson.sysml.mapper;

import com.fasterxml.jackson.databind.JsonNode;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.syson.sysml.AstConstant;
import org.eclipse.syson.sysml.Element;
import org.eclipse.syson.sysml.Membership;
import org.eclipse.syson.sysml.MembershipImport;
import org.eclipse.syson.sysml.SysmlPackage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements mapping logic specific to MembershipImport in SysML models from AST node.
 *
 * @author gescande
 */
public class MapperMembershipImport extends MapperVisitorInterface {

    private final Logger logger = LoggerFactory.getLogger(MapperMembershipImport.class);

    public MapperMembershipImport(final ObjectFinder objectFinder, final MappingState mappingState) {
        super(objectFinder, mappingState);
    }

    @Override
    public boolean canVisit(final MappingElement mapping) {
        return mapping.getSelf() != null && SysmlPackage.eINSTANCE.getMembershipImport().isSuperTypeOf(mapping.getSelf().eClass()) && mapping.getMainNode().has(AstConstant.TARGET_REF_CONST);
    }

    @Override
    public void mappingVisit(final MappingElement mapping) {
        this.logger.debug("Add MembershipImport to map for p  = " + mapping.getSelf());

        this.mappingState.toMap().add(new MappingElement(mapping.getMainNode().get(AstConstant.TARGET_REF_CONST), mapping.getSelf()));

        String importText = AstConstant.asCleanedText(mapping.getMainNode().get(AstConstant.TARGET_REF_CONST).get(AstConstant.TEXT_CONST));

        this.objectFinder.addImportMember(importText);

        this.mappingState.toResolve().add(mapping);

        MembershipImport eObject = (MembershipImport) mapping.getSelf();
        eObject.setDeclaredName(importText);
    }

    @Override
    public void referenceVisit(final MappingElement mapping) {

        JsonNode subElement = mapping.getMainNode().get(AstConstant.TARGET_REF_CONST);
        EObject referencedObject = this.objectFinder.findObject(mapping, subElement, SysmlPackage.eINSTANCE.getElement());

        Element target = (Element) referencedObject;

        MembershipImport eObject = (MembershipImport) mapping.getSelf();
        if (target == null) {
            this.logger.warn("Reference MembershipImport not found " + subElement);
            return;
        }

        this.logger.debug("Reference MembershipImport " + eObject + " to " + target);

        if (target instanceof Membership) {
            eObject.setImportedMembership((Membership) target);
        } else if (target.eContainer() instanceof Membership) {
            eObject.setImportedMembership((Membership) target.eContainer());
        } else if (target.eContainer() != null) {
            this.logger.warn(target.eContainer() + " Parent of " + target + " is not a Membership for " + eObject);
        } else {
            this.logger.warn("Parent of " + target + " is null for " + eObject);
            this.mappingState.toResolve().add(mapping);
        }

    }
}
