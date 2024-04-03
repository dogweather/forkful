---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:10.948469-07:00
description: "Hur man g\xF6r: I Fish Shell har du inte inbyggda kommandon som specifikt\
  \ \xE4r designade f\xF6r att tolka datum fr\xE5n str\xE4ngar. Ist\xE4llet f\xF6\
  rlitar du dig p\xE5 externa\u2026"
lastmod: '2024-03-13T22:44:38.347801-06:00'
model: gpt-4-0125-preview
summary: "I Fish Shell har du inte inbyggda kommandon som specifikt \xE4r designade\
  \ f\xF6r att tolka datum fr\xE5n str\xE4ngar."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

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
