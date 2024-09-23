-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
use Ada.Text_IO;

with AHTML;

procedure Tests is
   Basic_Test_Case : constant AHTML.Node := AHTML.Mk_Node ("meow");
   With_Attr : constant AHTML.Node := Basic_Test_Case.With_Attribute
      (AHTML.Mk_Attr ("a", "b"));

   With_Child : constant AHTML.Node := AHTML.With_Child
      (Basic_Test_Case, With_Attr);
begin
   Put_Line (AHTML.SU.To_String (AHTML.To_String (Basic_Test_Case)));
   Put_Line (AHTML.SU.To_String (AHTML.To_String (With_Attr)));
   Put_Line (AHTML.SU.To_String (AHTML.To_String (With_Child)));
end Tests;
