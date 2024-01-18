---
title:                "Att tolka ett datum från en sträng"
html_title:           "Elixir: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att analysera ett datum från en sträng är processen att ta en sträng med datuminformation och konvertera den till ett datumobjekt som programmeringsprogram kan bearbeta och manipulera. Det är vanligtvis ett viktigt steg vid datahantering och formatering inom utveckling av programvara.

## Hur gör man:

```Elixir
Date.from_iso8601("2020-05-15")
#=> ~D[2020-05-15]

DateTime.from_iso8601("2020-05-15T19:35:06.123Z")
#=> ~U[2020-05-15 19:35:06.123Z]

NaiveDateTime.decode("2020-05-15 19:35:06")
#=> {:ok, ~N[2020-05-15 19:35:06]}
```

## Fördjupning

Att analysera ett datum från en sträng har blivit en standardfunktion i många programmeringsspråk, men det var inte alltid så. Historiskt sett var det en manuell och komplicerad uppgift att konvertera datum från en sträng, och fel kunde ofta uppstå på grund av olika format och språkvariationer.

Elixir tillåter flera olika metoder för att konvertera datum från en sträng, inklusive `Date.from_iso8601/1`, `DateTime.from_iso8601/1` och `NaiveDateTime.decode/1`. Det finns också alternativ inklusive att använda tredjepartsbibliotek som `Timex` för hantering av datum och tider.

Implementeringen av funktionerna som tillåter analys av datum från en sträng i Elixir är optimerade för att vara snabba och effektiva, vilket är avgörande för prestanda i ett utvecklingsprojekt.

## Se även

För mer information om datum- och tidsmanipulering i Elixir, se följande källor:

- [Date Modulen](https://hexdocs.pm/elixir/Date.html)
- [DateTime Modulen](https://hexdocs.pm/elixir/DateTime.html)
- [NaiveDateTime Modulen](https://hexdocs.pm/elixir/NaiveDateTime.html)
- [Timex Biblioteket](https://hexdocs.pm/timex/readme.html)