---
title:                "Att få den aktuella datumen"
html_title:           "Elixir: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den aktuella datumet är en viktig del av programmering, eftersom det låter oss hantera och spåra tidsbaserade händelser i våra program. Detta inkluderar allt från schemaläggning av uppgifter till att beräkna ålder och hantera tidszoner.

## Hur du gör:
För att få den aktuella datumet i Elixir kan du använda funktionen `:calendar.local_time()`. Det är en inbyggd funktion som returnerar ett tupel med det aktuella datumet som första element och klockslaget som andra element. Exempelvis:

```Elixir
{:ok, date_time} = :calendar.local_time()
IO.puts("Idag är det #{date_time.date}")
```

Detta kommer att skriva ut den aktuella datumet i formatet DD/MM/YYYY. Om du behöver ett specifikt datum kan du också använda funktionen `:calendar.date_to_gregorian_days()` som tar emot ett datum som argument och returnerar det som antal dagar sedan det gregorianska kalendariet startade. Exempelvis:

```Elixir
today = Date.utc_today()
days_since_epoch = :calendar.date_to_gregorian_days(today)
IO.puts("Idag är det #{days_since_epoch} dagar sedan det gregorianska kalendariet startade.")
```

## Djupdykning:
För att få en bättre förståelse av hur den aktuella datumet fungerar i Elixir, är det viktigt att förstå bakgrunden. Elixir är byggt på Erlang som i sin tur är inspirerat av språket Prolog. I Prolog används datumet som ett sätt att matcha och söka efter data. Elixir har ärvt denna funktionalitet genom pattern matching, vilket låter oss jämföra datum på ett enkelt sätt.

En annan metod för att få den aktuella datumet är att använda biblioteket `Calendar` som erbjuder ett brett utbud av funktioner för att hantera datum och tider. Detta kan vara användbart om du behöver göra mer avancerade beräkningar eller arbeta med olika tidszoner.

## Se även:
- Officiell dokumentation för Elixir's `Calendar` bibliotek: https://hexdocs.pm/elixir/Calendar.html 
- En tutorial om hur man hanterar datum och tider i Elixir: https://elixirschool.com/sv/lessons/basics/dates/