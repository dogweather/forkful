---
title:    "Elixir: Att få den aktuella datumen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få den nuvarande datumet är en vanlig uppgift när man skriver kod, oavsett vilket programmeringsspråk man använder. Det kan vara användbart för att hålla koll på tider i en applikation eller för att lösa problem med datum och tid. I denna bloggpost kommer vi att gå igenom hur man får den aktuella datumet i Elixir.

## Hur man gör det
För att få den nuvarande datumet i Elixir använder du funktionen `Date.utc_today/0`. Det är viktigt att notera att den här funktionen returnerar ett "Date"-objekt, som innehåller information om årtal, månad och dag. För att få en enkel sträng av bara datumet kan du använda funktionen `to_string/1`. Här är ett exempel:

```Elixir
current_date = Date.utc_today()
IO.puts("Idag är det " <> Date.to_string(current_date))
```

Detta kommer att producera följande output:

```Elixir
Idag är det 2021-07-08
```

En annan användbar funktion är `DateTime.utc_now/0`, som returnerar den nuvarande tiden och datumet baserat på den universella koordinerade tiden (UTC). Här är ett exempel på hur man använder den:

```Elixir
current_datetime = DateTime.utc_now()
IO.puts("Klockan är " <> Time.to_string(current_datetime))
```

Detta kommer att ge output som liknar detta:

```Elixir
Klockan är 2021-07-08 14:30:00.123456Z
```

## Djupdykning
Det finns flera funktioner i Elixir för att hantera datum och tid, inklusive `Date`, `DateTime` och `NaiveDateTime`. Var och en har sina egna speciella egenskaper och användningsområden. Det är också viktigt att känna till att Elixir använder Erlangs `:calendar`-modul under huven för att hantera datum och tid.

En annan intressant funktion är `DateTime.from_iso8601/1`, som kan användas för att omvandla en sträng i ISO 8601-format till ett `DateTime`-objekt. Detta kan vara till nytta om du behöver konvertera datum och tid till ett annat format.

Det är också värt att nämna att Elixir erbjuder en rad hjälpprogram för att hantera datum och tid, såsom `NaiveDateTime.add/2` och `NaiveDateTime.diff/2`, som kan hjälpa dig att göra beräkningar med datum och tid.

## Se även
- [Officiell Elixir dokumentation för Date och Time modulerna](https://hexdocs.pm/elixir/Date.html)
- [Inbäddade Elixir funktioner för att hantera datum och tid](https://stackoverflow.com/questions/43998462/using-elixir-functions-to-handle-datetime-objects)
- [Erlang :calendar-modulen](https://erlang.org/doc/man/calendar.html)