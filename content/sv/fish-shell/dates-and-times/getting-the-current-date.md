---
aliases:
- /sv/fish-shell/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:33.226122-07:00
description: "Att h\xE4mta det aktuella datumet i programmering \xE4r en grundl\xE4\
  ggande uppgift som m\xF6jligg\xF6r att h\xE4mta och manipulera systemets datum-\
  \ och tidsdata. I\u2026"
lastmod: 2024-02-18 23:08:52.217789
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i programmering \xE4r en grundl\xE4ggande\
  \ uppgift som m\xF6jligg\xF6r att h\xE4mta och manipulera systemets datum- och tidsdata.\
  \ I\u2026"
title: "F\xE5 det aktuella datumet"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i programmering är en grundläggande uppgift som möjliggör att hämta och manipulera systemets datum- och tidsdata. I skriptning och automatiseringsuppgifter är det väsentligt för att generera tidsstämplar, schemalägga uppgifter och skapa loggar.

## Hur man gör:
Fish Shell använder externa kommandon som `date` för att hämta det aktuella datumet, vilket ger flexibilitet att formatera utdatan som behövs. Så här använder du det:

```fish
# Visa det aktuella datumet i standardformatet
echo (date)

# Exempel på utdata: Ons 25 Okt 2023 15:42:03 BST
```

För att anpassa datumets format kan du använda `+`-alternativet följt av formatidentifierare:

```fish
# Visa det aktuella datumet i YYYY-MM-DD-format
echo (date "+%Y-%m-%d")

# Exempel på utdata: 2023-10-25
```

För mer komplexa uppgifter, som att arbeta med tidsstämplar eller utföra datumaritmetik, förlitar sig Fish Shell på externa verktyg som `date` på grund av dess skriptnatur. Här är ett exempel på att få det aktuella UNIX-tidsstämpeln:

```fish
# Hämta det aktuella UNIX-tidsstämpeln
echo (date "+%s")

# Exempel på utdata: 1666710123
```

Och för att lägga till en dag till det aktuella datumet med `date`:

```fish
# Lägg till en dag till det aktuella datumet
echo (date -d "+1 day" "+%Y-%m-%d")

# Exempel på utdata: 2023-10-26
```

Notera: Exemplen använder `date`-kommandots alternativ som fungerar med GNU coreutils. Alternativ kan variera i andra miljöer som macOS, som använder BSD date-kommando som standard. Hänvisa alltid till `date --help` eller manualsidan för detaljer specifika till din miljö.
