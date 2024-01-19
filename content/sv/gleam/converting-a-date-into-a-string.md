---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

#förvandla datum till sträng i Gleam

## Vad & Varför?
Att omvandla ett datum till en sträng innebär att konvertera en datumrepresentation till ett läsbart textformat. Programmerare gör det oftast för att göra datumen lättare att visa och tolka för användare.

## Hur man gör:
Att konvertera ett datum till en sträng i Gleam är ganska enkelt. Låt oss titta på enkel kodsnutt.

```gleam
import gleam/date.{Date, from_iso_string}
import gleam/string_builder.{empty, append}

fn format_date(date: Date) -> String {
    let date_string_builder = empty
        |> append(date.year |> Int.to_string)
        |> append("-")
        |> append(date.month |> Int.to_string)
        |> append("-")
        |> append(date.day |> Int.to_string)
    string_builder.to_string(date_string_builder)
}

let date = from_iso_string("2023-01-01")
let string = format_date(date)
io.println(string)
```

Kör ovanstående kod skulle skriva ut `2023-01-01` som sträng.

## Djup Dykning
Att omvandla ett datum till en sträng har historiskt sett varit en vanlig koduppgift oavsett programmeringsspråk eller teknisk stack. I Gleam kan man enkelt utföra detta med hjälp av 'gleam/date' och 'gleam/string_builder' moduler. 

Alternativt kan man också använda 'DateTime.to_string' funktionen för samma uppgift, men det ger även tid, vilket kanske inte alltid är önskvärt.

Att konvertera datumet till en sträng har mycket att göra med maskinens interna representation av ett datumobjekt, vilket kan variera beroende på operativsystemet, språkets implementering och andra faktorer.

## Se även
- 'gleam/date' module i Gleam's standard library: https://hexdocs.pm/gleam_stdlib/gleam/date
- 'gleam/string_builder' module: https://hexdocs.pm/gleam_stdlib/gleam/string_builder
- Datetime formatting in Gleam: https://hexdocs.pm/gleam_stdlib/gleam/date_time
- Gleam's official guide: https://gleam.run/learn/