---
title:                "Arbeta med JSON"
date:                  2024-02-03T19:22:44.754851-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON i Fish Shell innebär att tolka och generera JSON-data, en vanlig uppgift för att konfigurera applikationer, API-interaktion och effektivisering av arbetsflöden i kommandotolken. Med tanke på JSON:s allestädes närvaro i webb- och applikationsutveckling, kan att bemästra dess manipulation direkt i skalet avsevärt förbättra automatiseringen och effektiviteten i hanteringen av data för programmerare.

## Hur man gör:

Fish Shell har i sig inte inbyggda verktyg för att tolka och generera JSON. Dock integreras den sömlöst med tredjepartsverktyg som `jq` för bearbetning av JSON. `jq` är en kraftfull och mångsidig kommandoradsprocess för JSON som låter dig skiva, filtrera, mappa och transformera strukturerad data med ett enkelt och uttrycksfullt språk.

### Tolkning av JSON med jq
För att tolka en JSON-fil och extrahera data med hjälp av `jq`:

```fish
# Antag att du har en JSON-fil med namnet 'data.json' med innehållet: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Exempel på utdata
"Fish Shell"
```

### Generering av JSON med jq
Att skapa JSON-innehåll från skalvariabler eller utdata:

```fish
# Skapa JSON-objekt från variabler
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Exempel på utdata
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrering av JSON-kollektioner
Anta att vi har en JSON-array med objekt i en fil med namnet `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
För att filtrera denna array endast för stabila versioner:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Exempel på utdata
"3.1.2"
"3.4.0"
```

Exemplen som tillhandahålls demonstrerar kraften i att integrera `jq` med Fish Shell för JSON-operationer. Att dra nytta av sådana verktyg berikar upplevelsen i skalet, vilket gör det till en formidabel miljö för hantering av moderna dataformat.
