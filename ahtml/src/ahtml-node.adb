--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body AHTML.Node is

   use Attrs_Vec;
   use Node_Vec;
   use SU;
   use Ada.Containers;

   function Null_Doc return Doc is
      ((Inner => Node_Vec.Empty_Vector));

   function Mk_Node (D : in out Doc; Name : String) return Node_Handle is
      (D.Mk_Node (SU.To_Unbounded_String (Name)));

   function Mk_Node (D : in out Doc; Name : SU.Unbounded_String)
      return Node_Handle
   is
      Attrs : Attrs_Vec.Vector;
      Children : Index_Vec.Vector;
      N : constant Node :=
         (Name => Name, Attrs => Attrs, Children => Children);
   begin
      D.Inner.Append (N);
      return D.Inner.Last_Index;
   end Mk_Node;

   function Mk_Attr (Key, Val : String) return Attr is
      (Key => SU.To_Unbounded_String (Key),
      Val => SU.To_Unbounded_String (Val));

   procedure With_Child (D : in out Doc; N, C : Node_Handle)
   is
      procedure Update (N : in out Node) is
      begin
         N.Children.Append (C);
      end Update;
   begin
      D.Inner.Update_Element (N, Update'Access);
   end With_Child;

   procedure With_Attribute (D : in out Doc; N : Node_Handle; A : Attr)
   is
      procedure Update (N : in out Node) is
      begin
         N.Attrs.Append (A);
      end Update;
   begin
      D.Inner.Update_Element (N, Update'Access);
   end With_Attribute;

   function To_String (D : Doc; N : Node_Handle) return SU.Unbounded_String
   is
      Target : constant Node := D.Inner (N);
      Tmp : SU.Unbounded_String := "<" & Target.Name;
      Have_Children : constant Boolean := Target.Children.Length /= 0;
   begin
      for Attr of Target.Attrs loop
         Tmp := @ & " " & Attr.Key & "=" & '"' & Attr.Val & '"';
      end loop;

      if Have_Children then
         Tmp := @ & ">";
      end if;

      for Child of Target.Children loop
         Tmp := @ & D.To_String (Child);
      end loop;

      if Have_Children then
         Tmp := @ & ("</" & Target.Name & ">");
      else
         Tmp := @ & "/>";
      end if;

      return Tmp;
   end To_String;

end AHTML.Node;