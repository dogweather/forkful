---
title:                "Att få den aktuella datumet"
html_title:           "Fish Shell: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att hämta den aktuella datumet är en vanlig uppgift för programmerare. Det innebär helt enkelt att få ut dagens datum och tidszon för att använda det i olika program eller skript. Det kan vara användbart för att spåra när ett program körs, eller för att organisera filer efter datum.

# Så här gör du:

```Fish Shell
date
```

När du kör kommandot ovan i Fish Shell kommer du att få ut dagens datum och tidszon på din dator. Om du vill formatera det på ett specifikt sätt kan du använda följande syntax:

```Fish Shell
date +"format"
```

Där "format" är den specifika formateringskoden du vill använda. Till exempel kan du använda "%d" för att få ut datumet i siffror, eller "%B" för att få ut månaden i textform. Här är några exempel:

```Fish Shell
date +"%d-%B-%Y" # kommer att ge ut formatet 07-Juni-2021
date +"%A, %d %B %Y" # kommer att ge ut formatet Måndag, 07 Juni 2021
```

# Djupdykning:

Att få ut dagens datum har blivit enklare med moderna operativsystem och språk, men det finns fortfarande flera olika metoder för att göra det. En alternativ metod är att använda det inbyggda datumobjektet i många programmeringsspråk, som till exempel "datetime" biblioteket i Python.

Fish Shell använder en extern kommando "date" för att hämta aktuella datumet, men det är fortfarande möjligt att skapa en egen inbyggd funktion för att göra detta med hjälp av kommandot "builtin". Detta skulle vara mer effektivt för stora mängder av begäranden till datumet, och skulle möjliggöra för programmerare att skapa en egen formateringsmetod.

# Se även:

- [Fish Shell dokumentation om "date"](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell dokumentation om "builtin"](https://fishshell.com/docs/current/cmds/builtin.html)