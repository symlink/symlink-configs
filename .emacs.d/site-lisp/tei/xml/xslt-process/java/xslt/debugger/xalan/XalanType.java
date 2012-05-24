/*
    XalanType.java

    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: August 1, 2002

    Copyright (C) 2002 Tony Addyman

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.
   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
   
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
 */

package xslt.debugger.xalan;

import org.apache.xpath.objects.XObject;

import xslt.debugger.Type;

public class XalanType extends Type
{
  public XalanType(int xalanType)
  {
    super(null);

    switch (xalanType) {
    case XObject.CLASS_BOOLEAN:
      type = Type.BOOLEAN;
      break;
    case XObject.CLASS_NUMBER:
      type = Type.NUMBER;
      break;
    case XObject.CLASS_STRING:
      type = Type.STRING;
      break;
    case XObject.CLASS_NODESET:
      type = Type.NODESET;
      break;
    case XObject.CLASS_UNKNOWN:
      type = Type.OBJECT;
      break;
    default:
      type = Type.ANY;
      break;
    }
  }
}
