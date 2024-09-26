--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body AHTML.Node is

   use Attrs_Vec;
   use Node_Vec;
   use SU;
   use Ada.Containers;

   function Mk_Node (Name : String) return Node is
      (Mk_Node (SU.To_Unbounded_String (Name)));

   function Mk_Node (Name : SU.Unbounded_String) return Node
   is
      Attrs : Attrs_Vec.Vector;
      Children : Node_Vec.Vector;
   begin
      return (Name => Name, Attrs => Attrs, Children => Children);
   end Mk_Node;

   function Mk_Attr (Key, Val : String) return Attr is
      (Key => SU.To_Unbounded_String (Key),
      Val => SU.To_Unbounded_String (Val));

   function With_Child (N : Node; C : Node) return Node is
      (Name => N.Name,
      Attrs => N.Attrs,
      Children => N.Children & new Node'(C));

   function With_Attribute (N : Node; A : Attr) return Node is
      (Name => N.Name, Attrs => N.Attrs & A, Children => N.Children);

   function To_String (N : Node) return SU.Unbounded_String
   is
      Tmp : SU.Unbounded_String := "<" & N.Name;
      Have_Children : constant Boolean := N.Children.Length /= 0;
   begin
      for Attr of N.Attrs loop
         Tmp := @ & " " & Attr.Key & "=" & '"' & Attr.Val & '"';
      end loop;

      if Have_Children then
         Tmp := @ & ">";
      end if;

      for Child of N.Children loop
         Tmp := @ & Child.To_String;
      end loop;

      if Have_Children then
         Tmp := @ & ("</" & N.Name & ">");
      else
         Tmp := @ & "/>";
      end if;

      return Tmp;
   end To_String;

end AHTML.Node;