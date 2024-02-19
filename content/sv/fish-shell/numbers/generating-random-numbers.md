---
aliases:
- /sv/fish-shell/generating-random-numbers/
date: 2024-01-27 20:34:00.048583-07:00
description: "Att generera slumpm\xE4ssiga tal \xE4r en grundl\xE4ggande uppgift i\
  \ programmering, anv\xE4nd f\xF6r allt fr\xE5n dataurval till spelutveckling. I\
  \ Fish Shell m\xF6jligg\xF6r\u2026"
lastmod: 2024-02-18 23:08:52.201521
model: gpt-4-0125-preview
summary: "Att generera slumpm\xE4ssiga tal \xE4r en grundl\xE4ggande uppgift i programmering,\
  \ anv\xE4nd f\xF6r allt fr\xE5n dataurval till spelutveckling. I Fish Shell m\xF6\
  jligg\xF6r\u2026"
title: Generera slumptal
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga tal är en grundläggande uppgift i programmering, använd för allt från dataurval till spelutveckling. I Fish Shell möjliggör användningen av systemverktyg och inbyggda funktioner för detta syfte att programmerare effektivt kan inkorporera slumpmässighet och variabilitet i skript och applikationer.

## Hur man gör:

Att generera ett slumpmässigt tal i Fish kan vara enkelt, med hjälp av kombinationen av systemverktyg och skal-funktioner. Nedan följer några exempel som visar hur man genererar slumpmässiga tal inom angivna intervall.

**Generera ett slumpmässigt tal mellan 0 och 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Exempel på utdata:**
```fish
42
```

**Generera ett slumpmässigt tal mellan två valfria tal, säg 50 och 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Exempel på utdata:**
```fish
103
```

**Använda random för att blanda en lista:**

Du kanske också vill slumpmässigt blanda elementen i en lista. Så här kan du göra det:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Exempel på utdata:**
```fish
C
A
E
D
B
```

Observera att utdatan kommer att variera varje gång du kör dessa kommandon på grund av slumpmässighetens natur.

## Djupdykning

Fish Shells `random`-funktion erbjuder ett lättanvänt gränssnitt för att generera pseudo-slumpmässiga tal. Internt, omsluter den systemnivås slumpmässiga nummergenereringsverktyg, vilket erbjuder ett portabelt sätt att introducera slumpmässighet i dina skript. Dock är det viktigt att komma ihåg att slumpmässigheten som `random` tillhandahåller är tillräcklig för de flesta skriptuppgifter men kanske inte uppfyller de kryptografiska säkerhetskraven för applikationer som behöver en högre grad av oförutsägbarhet.

För höginsatta säkerhetssammanhang, överväg att använda dedikerade verktyg eller programmeringsbibliotek designade för kryptografiska ändamål, vilka tillhandahåller starkare garantier för slumpmässighet. Dock, för allmänna skript och applikationer där de högsta säkerhetsstandarderna för slumpmässighet inte är ett krav, erbjuder Fish Shells `random`-funktion en bekväm och effektiv lösning.
