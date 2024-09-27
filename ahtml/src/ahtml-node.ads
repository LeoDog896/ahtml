--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Containers.Vectors;

with AHTML.Strings;

package AHTML.Node is

   type Node_Handle is private;
   type Doc is tagged private;

   type Attr is record
      Key : AHTML.Strings.Name;
      Val : AHTML.Strings.Cooked;
   end record;

   function Null_Doc return Doc;

   function Mk_Node (D : in out Doc; Name : String) return Node_Handle;
   function Mk_Node
      (D : in out Doc; Name : AHTML.Strings.Name)
      return Node_Handle;

   function Mk_Attr
      (Key : AHTML.Strings.Name; Val : AHTML.Strings.Cooked)
      return Attr;

   procedure With_Child (D : in out Doc; N, C : Node_Handle);
   procedure With_Attribute (D : in out Doc; N : Node_Handle; A : Attr);

   function To_String (D : Doc; N : Node_Handle)
      return AHTML.Strings.Raw;

private

   type Node_Handle is new Natural;

   package Attrs_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Attr);

   package Index_Vec is new Ada.Containers.Vectors
      (Index_Type => Node_Handle, Element_Type => Node_Handle);

   type Node is tagged record
      Name : AHTML.Strings.Name;
      Attrs : Attrs_Vec.Vector;
      Children : Index_Vec.Vector;
   end record;

   package Node_Vec is new Ada.Containers.Vectors
      (Index_Type => Node_Handle, Element_Type => Node);

   type Doc is tagged record
      Inner : Node_Vec.Vector;
   end record;

end AHTML.Node;
