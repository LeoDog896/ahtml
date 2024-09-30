--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Strings.Unbounded;

package AHTML.Strings is
   package SU renames Ada.Strings.Unbounded;

   type Raw is new SU.Unbounded_String;
   --  Raw is an Unbounded_String meant for unescaped XML and HTML.

   type Name is new SU.Unbounded_String;
   --  NAMES are a liberally interpreted version of the XML
   --  spec. I know, I'm sorry. Everything but angle brackets,
   --  quotes, and whitespace are allowed.

   type Cooked is new SU.Unbounded_String;
   --  Cooked is an immutable and heap-allocated string type.
   --  Characters that must be escaped for HTML are escaped as
   --  necessary.

   --  TODO: add dynamic predicates to check production rules

end AHTML.Strings;