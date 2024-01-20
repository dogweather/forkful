---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att konvertera ett datum till en sträng i programmering handlar om att ändra hur datat visas, utan att ändra själva informationen. Vi gör detta för att göra datat mer lättillgängligt och läsbart för användarna.

# Hur man gör:
Här är ett exempel på hur du konverterar ett `Date`-objekt till en sträng i Elixir:

```Elixir
date = Date.UTC(2021, 12, 25)
date_string = to_string(date)

IO.puts(date_string)
```
Utskriften kommer vara: `2021-12-25`

# Djupdykning
Historiskt sett har olika programmeringsspråk hanterat datums omvandling till strängar på olika sätt. Elixir använder `to_string` funktionen för att konvertera datumet till en ISO 8601-kompatibel sträng. 

Alternativt kan du använda `Date.to_iso8601/1` funktionen, vilket också ger samma resultat:

```Elixir
date = Date.UTC(2021, 12, 25)
date_string = Date.to_iso8601(date)

IO.puts(date_string)
```

Implementationen av konverteringen mellan datum och strängar i Elixir är snabb, standardiserad och pålitlig, vilket gör den till ett bra verktyg för daglig programmering.

# Se även:
För mer om att jobba med datum och tider i Elixir, checka in följande länkar: 
- Elixir offciella dokumentation på `Date`: https://hexdocs.pm/elixir/Date.html
- Elixir School's lektion om datum och tider: https://elixirschool.com/en/lessons/basics/date-time/ 
- A deep dive into Elixir's new calendar types: https://blog.appsignal.com/2019/02/19/elixir-almanac-date-time-and-calendar.html