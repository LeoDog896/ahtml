--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body AHTML.Strings is

   function Cook (Text : String) return Cooked is
   begin
      --  LOL. LMAO.
      return (Inner => Raw (SU.To_Unbounded_String (Text)));
   end Cook;

   function Denote (Text : String) return Name is
   begin
      return (Inner => Raw (SU.To_Unbounded_String (Text)));
   end Denote;

   function Unwrap (C : Cooked) return Raw is (C.Inner);
   function Unwrap (N : Name)   return Raw is (N.Inner);

end AHTML.Strings;