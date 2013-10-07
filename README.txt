=====================
(Very VERY early) Beginnings of a Common Lisp Simcity-ish clone. Gamedev sundays.

* Needs lots of work :p

Screenshot (current)
https://www.dropbox.com/s/vkkalv2kas5vqo6/common-city.png

http://www.youtube.com/watch?v=SNxaLiwHy50
(Early video. Currently it's a bit more complete.)

Controls:

- H -> Residential Zone
- C -> Commercial Zone
- I -> Industrial Zone
- P -> Police Department
- F -> Fire Department
- N -> Nuclear Plant
- O -> Coal Plant
- S -> Seaport
- U -> Stadium
- A -> Airport
- D -> Dozer
- R -> Road
- T -> Rail
- W -> Wire
- G -> Garden
- X -> Reset world
- Q -> Quit

Tested on OS X 10.8.4 and Ubuntu 12.04.

Run:
(ql:quickload :common-city)
(common-city::main)

This repository was created for participation in Lisp in Summer Projects (http://lispinsummerprojects.org/).

Work is continued from my own initial code at https://github.com/jsmpereira/cl-simcity. It started with a group
of webdev guys with no gamedev experience getting together on sundays to make games. Basically trying to figure stuff out, each using their favorite programming language.

The code is not based on any existing city simulator. As mentioned, it's just me trying to figure how to do things.
I make use however of some resources from Micropolis (http://wiki.laptop.org/go/Micropolis), namely sprites and some sounds, hence the notice below, README.MICROPOLIS and COPYING.

========================================================================

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.  You should have received a
copy of the GNU General Public License along with this program.  If
not, see <http://www.gnu.org/licenses/>.

            ADDITIONAL TERMS per GNU GPL Section 7

No trademark or publicity rights are granted.  This license does NOT
give you any right, title or interest in the trademark SimCity or any
other Electronic Arts trademark.  You may not distribute any
modification of this program using the trademark SimCity or claim any
affliation or association with Electronic Arts Inc. or its employees.
Any propagation or conveyance of this program must include this
copyright notice and these terms.

If you convey this program (or any modifications of it) and assume
contractual liability for the program to recipients of it, you agree
to indemnify Electronic Arts for any liability that those contractual
assumptions impose on Electronic Arts.

You may not misrepresent the origins of this program; modified
versions of the program must be marked as such and not identified as
the original program.

This disclaimer supplements the one included in the General Public
License.  TO THE FULLEST EXTENT PERMISSIBLE UNDER APPLICABLE LAW, THIS
PROGRAM IS PROVIDED TO YOU "AS IS," WITH ALL FAULTS, WITHOUT WARRANTY
OF ANY KIND, AND YOUR USE IS AT YOUR SOLE RISK.  THE ENTIRE RISK OF
SATISFACTORY QUALITY AND PERFORMANCE RESIDES WITH YOU.  ELECTRONIC ARTS
DISCLAIMS ANY AND ALL EXPRESS, IMPLIED OR STATUTORY WARRANTIES,
INCLUDING IMPLIED WARRANTIES OF MERCHANTABILITY, SATISFACTORY QUALITY,
FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT OF THIRD PARTY
RIGHTS, AND WARRANTIES (IF ANY) ARISING FROM A COURSE OF DEALING,
USAGE, OR TRADE PRACTICE.  ELECTRONIC ARTS DOES NOT WARRANT AGAINST
INTERFERENCE WITH YOUR ENJOYMENT OF THE PROGRAM; THAT THE PROGRAM WILL
MEET YOUR REQUIREMENTS; THAT OPERATION OF THE PROGRAM WILL BE
UNINTERRUPTED OR ERROR-FREE, OR THAT THE PROGRAM WILL BE COMPATIBLE
WITH THIRD PARTY SOFTWARE OR THAT ANY ERRORS IN THE PROGRAM WILL BE
CORRECTED.  NO ORAL OR WRITTEN ADVICE PROVIDED BY ELECTRONIC ARTS OR
ANY AUTHORIZED REPRESENTATIVE SHALL CREATE A WARRANTY.  SOME
JURISDICTIONS DO NOT ALLOW THE EXCLUSION OF OR LIMITATIONS ON IMPLIED
WARRANTIES OR THE LIMITATIONS ON THE APPLICABLE STATUTORY RIGHTS OF A
CONSUMER, SO SOME OR ALL OF THE ABOVE EXCLUSIONS AND LIMITATIONS MAY
NOT APPLY TO YOU.
