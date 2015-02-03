(*
    Ocveralls - Generate JSON file for http://coveralls.io API
                from Bisect (http://bisect.x9c.fr) data.

    Copyright (C) 2015 Julien Sagot

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(** Overlay on [ezjsonm]. *)

let unit : Ezjsonm.value
  = `Null

let string : string -> Ezjsonm.value
  = Ezjsonm.string

let int : int -> Ezjsonm.value
  = Ezjsonm.int

let list : ('a -> Ezjsonm.value) -> 'a list -> [> Ezjsonm.t]
  = fun f l -> `A (List.map f l)

let dict : (string * Ezjsonm.value) list -> [> Ezjsonm.t]
  = fun x -> `O x

let to_channel : out_channel -> Ezjsonm.t -> unit
 = Ezjsonm.to_channel ~minify:true
