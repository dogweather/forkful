---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:10.093565-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att omvandla text till ett datumformat som programmet kan förstå och hantera. Programmerare gör detta för att kunna bearbeta och analysera datumdata som kommer i textform - tänk användarinmatning eller filer externt.

## How to:
```gleam
import gleam/erlang
import gleam/calendar.{Date, ParseError}
import gleam/string

pub fn main() {
  let raw_date = "2023-03-14"
  let parsed_date = string.split(raw_date, "-")
                      |> parse_date_parts

  case parsed_date {
    Ok(date) -> erlang.display(date)
    Error(_) -> erlang.display("Ogiltigt datumformat")
  }
}

fn parse_date_parts(parts: List(String)) -> Result(Date, ParseError) {
  match parts {
    [year, month, day] ->
      case int_of_string(year) {
        Ok(y) -> 
          case int_of_string(month), int_of_string(day) {
            Ok(m), Ok(d) -> Date.new(y, m, d)
            _, _ -> Error(ParseError)
          }
        Error(_) -> Error(ParseError)
      }
    _ -> 
      Error(ParseError)
  }
}

fn int_of_string(value: String) -> Result(Int, Nil) {
  try int_from_string(value)
}
```
Exempelutskrift: `Ok(#Date<2023-03-14>)`

## Deep Dive
Datumtolkning är grundläggande i programvaruutveckling och särskilt relevant i situationer där system måste stödja flera lokaler och datumformat. Historiskt sett har detta varit en källa till buggar och missförstånd, med problem från år 2000 (Y2K) som det mest kända. Alternativ till Gleam för datumtolkning inkluderar bibliotek i andra språk som Moment.js för JavaScript eller DateTime i Elixir. Implementeringsdetaljer beror ofta på systemets lokala inställningar och hur strängdatumet är formaterat.

## See Also
- [Gleam's standard library documentation](https://hexdocs.pm/gleam_stdlib/)
- [Erlang's calendar module](http://erlang.org/doc/man/calendar.html)
- [Understanding Y2K](https://en.wikipedia.org/wiki/Year_2000_problem)