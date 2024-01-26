---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:33.606375-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är en process där ditt PHP-skript skapar ett nummer som inte kan förutsägas. Programmerare behöver det för allt från att säkra data (kryptografi) till att simulerar händelser (spel, tester).

## How to:
PHP har inbyggda funktioner för att generera slumpmässiga tal. Här är några snabba exempel:

```PHP
<?php
// Enkelt slumptal
echo random_int(0, 100); // Ger: ett tal mellan 0 och 100

// Kryptografiskt säkert slumptal
echo random_bytes(10); // Ger: en sträng av 10 slumpmässigt genererade bytes

// Slumptal för hjälp av mt_rand (snabbare men mindre säkert)
echo mt_rand(0, 100); // Ger: ett tal mellan 0 och 100
?>
```

## Deep Dive
Slumptalsgenerering i PHP har kommit långt. `rand()` användes mycket i början men är inte rekommenderat idag p.g.a. dess förutsägbarhet. `mt_rand()` är en förbättring men för säkerhetskrävande uppgifter bör `random_int()` eller `random_bytes()` användas eftersom de genererar kryptografiskt säkra tal.

Ett alternativ utanför de inbyggda PHP-funktionerna är att använda biblioteket `random_compat` som back-portar `random_int()` och `random_bytes()` till äldre PHP-versioner.

Det är viktigt när du implementerar slumpmässiga tal att välja rätt verktyg för jobbet. Är prestanda nyckeln, eller säkerhet? Det är en avvägning du måste göra.

## Se Även
- PHPs officiella dokumentation för [random_int()](https://www.php.net/manual/en/function.random-int.php)
- PHPs officiella dokumentation för [random_bytes()](https://www.php.net/manual/en/function.random-bytes.php)
- random_compat GitHub-repo: [https://github.com/paragonie/random_compat](https://github.com/paragonie/random_compat)
- En guide om [PHPs random-funktioner](https://www.php.net/manual/en/book.random.php)
