--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
use Ada.Text_IO;

with AHTML.Node; use AHTML.Node;

procedure Tests is
   Basic_Test_Case : constant AHTML.Node.Node := AHTML.Node.Mk_Node ("meow");
   With_Attr : constant AHTML.Node.Node := Basic_Test_Case.With_Attribute
      (AHTML.Node.Mk_Attr ("a", "b"));

   With_Child : constant AHTML.Node.Node := AHTML.Node.With_Child
      (Basic_Test_Case, With_Attr);
begin
   Put_Line (AHTML.Node.SU.To_String (AHTML.Node.To_String (Basic_Test_Case)));
   Put_Line (AHTML.Node.SU.To_String (AHTML.Node.To_String (With_Attr)));
   Put_Line (AHTML.Node.SU.To_String (AHTML.Node.To_String (With_Child)));
end Tests;
