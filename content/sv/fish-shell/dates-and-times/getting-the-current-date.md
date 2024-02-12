---
title:                "Få det aktuella datumet"
aliases:
- /sv/fish-shell/getting-the-current-date/
date:                  2024-02-03T19:09:33.226122-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
