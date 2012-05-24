/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;


/**
 * 'cancel_trace_methods' command.
 * <p>
 *
 * <b>Syntax: </b>
 * <pre>
 * cancel_trace_methods <u>requestID</u>
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>requestID</u> is returned in the trace methods reply
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class CancelTraceMethods extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 1)
      throw new JDEException("Insufficient arguments");

    deleteIdentifiableRequest(Etc.safeGetLong
				  (args.remove(0), "request ID"));
    
    jde.signalCommandResult(procID, cmdID);
  }

  public Object clone() {return new CancelTraceMethods();}
  
} // CancelTraceMethods

/*
 * $Log: CancelTraceMethods.java,v $
 * Revision 1.1  2001/03/24 05:48:39  paulk
 * Initial version.
 *
 *
 */

// End of CancelTraceMethods.java
