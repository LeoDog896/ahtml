--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Strings.Unbounded;

package AHTML.Strings is
   package SU renames Ada.Strings.Unbounded;

   type Raw is new SU.Unbounded_String;
   --  Raw is an Unbounded_String meant for unescaped HTML and
   --  HTML fragments. [TODO: make this a subtype]

   type Name is tagged private;
   --  Names are exclusively ASCII alphanumerics (WHATWG 13.1.2).

   type Cooked is tagged private;
   --  Cooked is an immutable and heap-allocated string type.
   --  Characters that must be escaped for HTML are escaped as
   --  necessary.

   function Cook (Text : String) return Cooked;
   function Denote (Text : String) return Name;

   function Unwrap (N : Name) return Raw;
   function Unwrap (C : Cooked) return Raw;

private

   type Name is tagged record
      Inner : Raw;
   end record;

   type Cooked is tagged record
      Inner : Raw;
   end record;

end AHTML.Strings;