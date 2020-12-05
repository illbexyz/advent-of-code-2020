open Base
open Stdio

let filepath = "day04/input.txt"

module Passport = struct
  type t = {
    birth_year : int option;
    issue_year : int option;
    expiration_year : int option;
    height : string option;
    hair_color : string option;
    eye_color : string option;
    passport_id : string option;
    country_id : string option;
  }

  let empty =
    {
      birth_year = None;
      issue_year = None;
      expiration_year = None;
      height = None;
      hair_color = None;
      eye_color = None;
      passport_id = None;
      country_id = None;
    }

  let of_list (alist : (string * string) list) : t =
    List.fold alist ~init:empty ~f:(fun pass (k, v) ->
        match k with
        | "byr" -> { pass with birth_year = Some (Int.of_string v) }
        | "iyr" -> { pass with issue_year = Some (Int.of_string v) }
        | "eyr" -> { pass with expiration_year = Some (Int.of_string v) }
        | "hgt" -> { pass with height = Some v }
        | "hcl" -> { pass with hair_color = Some v }
        | "ecl" -> { pass with eye_color = Some v }
        | "pid" -> { pass with passport_id = Some v }
        | "cid" -> { pass with country_id = Some v }
        | _ -> pass)

  let validate_birth_year year = year >= 1920 && year <= 2002

  let validate_issue_year year = year >= 2010 && year <= 2020

  let validate_expiration_year year = year >= 2020 && year <= 2030

  let validate_height height =
    String.length height > 2
    &&
    let height_value =
      String.prefix height (String.length height - 2) |> Int.of_string
    in
    let measure_unit = String.suffix height 2 in
    match (height_value, measure_unit) with
    | cm, "cm" -> cm >= 150 && cm <= 193
    | inc, "in" -> inc >= 59 && inc <= 76
    | _ -> false

  let validate_hair_color color =
    let has_hash = Char.equal color.[0] '#' in
    let suffix = String.suffix color 1 in
    let is_correct_char c =
      match c with
      | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> true
      | c when Char.is_digit c -> true
      | _ -> false
    in
    let is_correct = String.for_all suffix ~f:is_correct_char in
    has_hash && is_correct

  let validate_eye_color = function
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

  let validate_passport_id id =
    let is_length_valid = String.length id = 9 in
    let is_content_valid = String.for_all id ~f:Char.is_digit in
    is_length_valid && is_content_valid

  let validate_country_id _id = true

  let is_valid_p1 (passport : t) =
    match passport with
    | {
     birth_year = Some _;
     issue_year = Some _;
     expiration_year = Some _;
     height = Some _;
     hair_color = Some _;
     eye_color = Some _;
     passport_id = Some _;
     country_id = _;
    } ->
        true
    | _ -> false

  let is_valid_p2 (passport : t) =
    match passport with
    | {
     birth_year = Some byr;
     issue_year = Some iyr;
     expiration_year = Some eyr;
     height = Some hgt;
     hair_color = Some hcl;
     eye_color = Some ecl;
     passport_id = Some pid;
     country_id = _;
    } ->
        [
          validate_birth_year byr;
          validate_issue_year iyr;
          validate_expiration_year eyr;
          validate_height hgt;
          validate_hair_color hcl;
          validate_eye_color ecl;
          validate_passport_id pid;
        ]
        |> List.for_all ~f:(Bool.equal true)
    | _ -> false
end

module Parser = struct
  let parse_field s =
    match String.split ~on:':' s with
    | [ key; value ] -> (key, value)
    | _ -> failwith "Parse error: missing key or value"

  let parse_passports lines =
    List.group lines ~break:(fun x _ -> String.equal x "")
    |> List.map ~f:(List.concat_map ~f:(String.split ~on:' '))
    |> List.map ~f:(List.filter ~f:(fun s -> s |> String.equal "" |> not))
    |> List.map ~f:(List.map ~f:parse_field)
    |> List.map ~f:Passport.of_list
end

let run () =
  let lines = In_channel.read_lines filepath in
  let passports = Parser.parse_passports lines in
  let valid_passports_count_p1 =
    List.map passports ~f:Passport.is_valid_p1 |> List.count ~f:(fun x -> x)
  in
  let valid_passports_count_p2 =
    List.map passports ~f:Passport.is_valid_p2 |> List.count ~f:(fun x -> x)
  in
  Ok (valid_passports_count_p1, valid_passports_count_p2)
