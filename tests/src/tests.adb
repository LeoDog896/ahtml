--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
use Ada.Text_IO;

with AHTML.Node; use AHTML.Node;
with AHTML.Strings;

procedure Tests is
   Doc : AHTML.Node.Doc := AHTML.Node.Null_Doc;
   Root : constant AHTML.Node.Node_Handle := Doc.Mk_Node ("html");
   Body_Node : constant AHTML.Node.Node_Handle := Doc.Mk_Node ("body");

   Attr : constant AHTML.Node.Attr :=
      AHTML.Node.Mk_Attr
         (AHTML.Strings.To_Unbounded_String ("a"),
         AHTML.Strings.To_Unbounded_String ("b"));
begin
   Put_Line (AHTML.Strings.To_String (Doc.To_String (Root)));

   Doc.With_Child (Root, Body_Node);
   Put_Line (AHTML.Strings.To_String (Doc.To_String (Root)));

   Doc.With_Attribute (Body_Node, Attr);
   Put_Line (AHTML.Strings.To_String (Doc.To_String (Root)));

   Doc.With_Doctype (AHTML.Strings.To_Unbounded_String ("html"));
   Put_Line (AHTML.Strings.To_String (Doc.To_String (Root)));
end Tests;
