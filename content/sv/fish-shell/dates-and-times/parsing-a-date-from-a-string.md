---
title:                "Analysera ett datum från en sträng"
aliases:
- /sv/fish-shell/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:10.948469-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att extrahera datuminformation kodad i strängar och omvandla den till ett strukturerat format som programmeringsmiljöer kan känna igen och manipulera. Programmerare gör detta för att möjliggöra operationer såsom datumjämförelse, aritmetik, formatering och lokalisering, vilket är nödvändigt för att effektivt hantera schemaläggning, tidsstämplar och historiska data i mjukvara.

## Hur man gör:
I Fish Shell har du inte inbyggda kommandon som specifikt är designade för att tolka datum från strängar. Istället förlitar du dig på externa verktyg som `date` (tillgängligt i Linux och macOS) eller använder dig av populära tredjepartsverktyg som `GNU date` för mer komplex tolkning. Så här går du tillväga:

**Använda `date` med Fish:**

För att tolka en datumsträng i formatet "ÅÅÅÅ-MM-DD" kan du använda `date`-kommandot med `-d` (eller `--date` för GNU date) alternativet följt av strängen. `+`-alternativet används för att formatera utdatan.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Utdata: Lördag, 01 April 2023
```

För macOS (som kräver ett annat format för `-j` och `-f` flaggorna):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Utdata: Lördag, 01 April 2023
```

**Använda GNU `date` för komplex tolkning:** 

GNU `date` är mer flexibelt med strängformat. Det kan automatiskt detektera många vanliga datumsträngsformat utan att explicit ange inmatningsformatet:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Utdata: 2023-04-01 14:00:00
```

Dock, när man arbetar med datumsträngar som kanske inte automatiskt känns igen eller när exakt kontroll över inmatningsformatet behövs, stöds inte direkt angivning av inmatningsformatet med GNU `date`. I sådana fall, överväg att förbehandla strängen eller använda ett annat verktyg som är utformat för mer komplexa datumtolkningsrutiner.
