--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
use Ada.Text_IO;

with AHTML.Node; use AHTML.Node;

procedure Tests is
   Doc : AHTML.Node.Doc := AHTML.Node.Null_Doc;
   Root : constant AHTML.Node.Node_Handle := Doc.Mk_Node ("html");
   Body_Node : constant AHTML.Node.Node_Handle := Doc.Mk_Node ("body");
begin
   Put_Line (AHTML.Node.SU.To_String (Doc.To_String (Root)));

   Doc.With_Child (Root, Body_Node);
   Put_Line (AHTML.Node.SU.To_String (Doc.To_String (Root)));
   --  Put_Line (AHTML.Node.SU.To_String (AHTML.Node.To_String (With_Child)));
end Tests;
