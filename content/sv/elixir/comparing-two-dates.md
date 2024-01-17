---
title:                "Jämförande av två datum"
html_title:           "Elixir: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Jämföra två datum är en vanlig uppgift för många programmerare, särskilt inom datum- och tidsbehandling. Det kan användas för att kontrollera giltigheten av användarens inmatning, hantera schemaläggning eller sortera data i en databas. Det är en viktig del av många applikationer för att säkerställa korrekt och effektiv användning av tid.

## Så här gör du:
```elixir
date1 = ~D[2021-07-01] 
date2 = ~D[2021-08-01]
```
```elixir 
date1 <= date2 
#true
```
Det är enkelt att jämföra två datum i Elixir genom att helt enkelt använda jämförelseoperatorer som <, >, <= eller >= på de två datumobjekten. Elixir har inbyggda funktioner för att skapa och hantera datum- och tidsobjekt, vilket gör det enkelt att utföra denna uppgift.

## Djupdykning:
Att jämföra två datum är en vanlig uppgift över många programmeringsspråk, men Elixir har vissa unika egenskaper som underlättar denna uppgift. Elixir använder sig av Erlangs inbyggda modul för datum- och tidsbehandling, som bygger på den berömda kalendersystemet Gregorianska kalendern. Detta gör det enkelt för Elixir-utvecklare att hantera datum och tider med precision och korrekthet.

Förutom att använda jämförelseoperatorer finns det också alternativa sätt att jämföra två datum i Elixir, såsom genom användning av funktionen `Date.compare/2` som returnerar en negativ, noll eller positiv siffra som motsvarar relationen mellan de två datumen. Det finns också andra tidsrelaterade funktioner som kan användas i kombination för att utföra komplexa jämförelser.

## Se även:
- Elixir date och tid-dokumentation: https://hexdocs.pm/elixir/DateTime.html
- Elixir offentliga moduler för datum- och tidshantering: http://www.erlang.org/doc/man/calendar.html