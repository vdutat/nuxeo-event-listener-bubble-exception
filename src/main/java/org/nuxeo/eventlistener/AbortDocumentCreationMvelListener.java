package org.nuxeo.eventlistener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.automation.OperationContext;
import org.nuxeo.ecm.automation.core.scripting.Expression;
import org.nuxeo.ecm.automation.core.scripting.Scripting;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;
import org.nuxeo.runtime.api.Framework;
import org.nuxeo.runtime.services.config.ConfigurationService;

public class AbortDocumentCreationMvelListener extends AbortDocumentCreationListener {

    private static final Log LOG = LogFactory.getLog(AbortDocumentCreationMvelListener.class);

    protected String mvelExpressionPropertyName = "nuxeo.listener.abortDocumentCreation.expression";

	@Override
	protected boolean documentComplies(DocumentEventContext ctx) {
        DocumentModel doc = ctx.getSourceDocument();
        ConfigurationService service = Framework.getService(ConfigurationService.class);
        String expressionStr = service.getProperty(getMvelExpressionPropertyName());
        if (expressionStr != null) {
            LOG.debug("expression: " + expressionStr);
            Expression expression = Scripting.newExpression(expressionStr);
            setMessage("expression [" + expressionStr + "] returned 'false'");
            if (expression != null) {
                OperationContext opCtx = new OperationContext(ctx.getCoreSession());
                opCtx.setInput(doc);
                opCtx.getVars().put("eventContext", ctx);
                return (boolean) expression.eval(opCtx);
            } else {
                return true;
            }
        } else {
        	LOG.error(String.format("Unable to retrieve configuration variable '%s'", getMvelExpressionPropertyName()));
        }
		return true;
	}

	public String getMvelExpressionPropertyName() {
		return mvelExpressionPropertyName;
	}

	public void setMvelExpressionPropertyName(String mvelExpressionPropertyNmae) {
		mvelExpressionPropertyName = mvelExpressionPropertyNmae;
	}

}
