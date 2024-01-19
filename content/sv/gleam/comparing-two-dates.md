---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att jämföra två datum är processen där ett program avgör om ett datum är tidigare, senare eller samma som ett annat, baserat på en kalendertid. Programmerare gör detta för att kontrollera tidsskillnader, ordna data, och hantera tid-relaterade logik inom deras applikationer.

## Hur gör man:

Här är ett exempel på hur du jämförar två datum i Gleam:

```Gleam
import gleam/date

fn main() {
  let date1 = date.from_tuple(2020, 10, 1)
  let date2 = date.from_tuple(2021, 10, 1)

  let result = date.compare(date1, date2)

  case result {
    Ok(Equal) ->
      print("Datumen är desamma.")
    Ok(Greater) ->
      print("Det första datumet är senare.")
    Ok(Less) ->
      print("Det andra datumet är senare.")
    Error(err) ->
      print("Ett fel upptäcktes: ", err)
  }
}
```
Exemplet ovan skapar två datum och jämför dem. Resultatet av jämförelsen sammanfattas sedan i terminalen.

## Djupdykning

Historiskt sett har programmerare jämfört datum på flera sätt, från enkel år, månad, dag jämförelse till att omvandla datum till sekunder sedan en viss punkt (ex: Unix epok tid). Gleam erbjuder en inbyggd funktion för datumjämförelse, vilket förenklar processen betydligt.

Det finns alternativ till att använda `date.compare`. Du kan till exempel konvertera datumen till Unix-tid och jämföra dessa värden, men det kan bli mer komplicerat och utrymmeskrävande.

Gleam använder Elixir's inbyggda `Date.compare` funktion under huven. Funktionen ger tre möjliga resultat: `Equal`, `Less`, och `Greater`, vilket gör det lätt att bestämma vilket datum som är tidigare, senare eller om de är desamma.

## Se också

Se följande länkar för mer information och alternativa metoder:

- Gleam's officiella dokument om datum: https://gleam.run/stdlib/date/
- Elixir's officiella dokument om datumjämförelse: https://hexdocs.pm/elixir/Date.html#compare/2
- En bra artikel om datumjämförelse i allmänhet: https://www.w3schools.com/js/js_date_methods_diff.asp