---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:39.427792-07:00
description: "Att skriva tester i Fish Shell inneb\xE4r att skapa skript som automatiskt\
  \ k\xF6r din kod f\xF6r att validera dess beteende mot f\xF6rv\xE4ntade resultat.\
  \ Denna praxis\u2026"
lastmod: '2024-03-11T00:14:11.742471-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i Fish Shell inneb\xE4r att skapa skript som automatiskt\
  \ k\xF6r din kod f\xF6r att validera dess beteende mot f\xF6rv\xE4ntade resultat.\
  \ Denna praxis\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Fish Shell innebär att skapa skript som automatiskt kör din kod för att validera dess beteende mot förväntade resultat. Denna praxis är avgörande eftersom den säkerställer att dina shellskript fungerar som avsett, fångar fel tidigt och gör underhåll enklare.

## Hur:

Fish har inte ett inbyggt testramverk som vissa andra programmeringsmiljöer. Du kan dock skriva enkla testskript som använder påståenden för att kontrollera funktionernas beteende. Dessutom kan du utnyttja tredjepartsverktyg som `fishtape` för en mer omfattande testsamling.

### Exempel 1: Grundläggande Testskript

Låt oss börja med en grundläggande funktion i Fish som beräknar summan av två tal:

```fish
function add --description 'Lägg till två nummer'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Du kan skriva ett grundläggande testskript för denna funktion så här:

```fish
function test_add
    set -l resultat (add 3 4)
    if test $resultat -eq 7
        echo "test_add lyckades"
    else
        echo "test_add misslyckades"
    end
end

test_add
```

När du kör detta skript skulle utdatan bli:

```
test_add lyckades
```

### Exempel 2: Använda Fishtape

För en mer robust testlösning kan du använda `fishtape`, en TAP-producera testkörare för Fish.

Först, installera `fishtape` om du inte redan har gjort det:

```fish
fisher install jorgebucaran/fishtape
```

Nästa, skapa en testfil för din `add` funktion, t.ex., `add_test.fish`:

```fish
test "Att lägga till 3 och 4 ger 7"
    set resultat (add 3 4)
    echo "$resultat" | fishtape
end
```

För att köra testet, använd följande kommando:

```fish
fishtape add_test.fish
```

Exempel på utdata kan se ut som:

```
TAP version 13
# Att lägga till 3 och 4 ger 7
ok 1 - test_add lyckades
```

Detta meddelar dig att testet lyckades. `fishtape` möjliggör strukturering av mer detaljerade tester och ger informativ utdata, vilket underlättar felsökning och omfattande testtäckning för dina Fish-skript.
