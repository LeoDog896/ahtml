--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package AHTML is
   --  TODO: wrap this type and HTML escape what needs to be escaped
   package SU renames Ada.Strings.Unbounded;

   type Node is tagged private;

   type Attr is record
      Key : SU.Unbounded_String;
      Val : SU.Unbounded_String;
   end record;

   function Mk_Node (Name : String) return Node;

   function Mk_Attr (Key, Val : String) return Attr;

   function Mk_Node
      (Name : SU.Unbounded_String)
      return Node;

   function With_Child (N : Node; C : Node) return Node;

   function With_Attribute (N : Node; A : Attr) return Node;

   function To_String (N : Node) return SU.Unbounded_String;

private

   type Node_Acc is access all Node;

   package Attrs_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Attr);

   package Node_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Node_Acc);

   type Node is tagged record
      Name : SU.Unbounded_String;
      Attrs : Attrs_Vec.Vector;
      Children : Node_Vec.Vector;
   end record;

end AHTML;
