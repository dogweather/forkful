---
date: 2024-01-20 17:56:38.322726-07:00
description: "Att l\xE4sa kommandoradsargument inneb\xE4r att f\xE5nga text som skickas\
  \ till ett PHP-script fr\xE5n terminalen. Programmerare g\xF6r detta f\xF6r att\
  \ till\xE5ta interaktion\u2026"
lastmod: '2024-03-13T22:44:38.011620-06:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa kommandoradsargument inneb\xE4r att f\xE5nga text som skickas\
  \ till ett PHP-script fr\xE5n terminalen."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## How to:
Ett enkelt PHP-script för att läsa kommandoradsargument kan se ut så här:

```PHP
<?php
if ($argc > 1) {
    echo "Hej, " . $argv[1] . "!\n";
} else {
    echo "Hej, okänd användare!\n";
}
?>
```

Om du kör detta script med:

```
php script.php Johan
```

Skulle utmatningen bli:

```
Hej, Johan!
```

## Deep Dive
PHP har stöd för kommandoradsinteraktion sedan de tidiga versionerna. Funktionerna `$argc` och `$argv` är inbyggda variabler som automatiskt fylls med antalet argument och argumentens värden.

Alternativ till `$argc` och `$argv` inkluderar att använda `getopt()` för mer avancerad argumenthantering. `getopt()` tillåter flaggor och långa optioner som gör skripten mer användarvänliga.

Implementationen av kommandoradsargument i PHP är ganska rak på sak. `$argc` (argument count) lagrar antalet argument och `$argv` (argument vector) är en array som innehåller själva argumenten, där `$argv[0]` alltid är skriptets namn.

## See Also
- PHP:s officiella dokumentation om kommandoradsanvändning: https://www.php.net/manual/en/features.commandline.php
- Artiklar om `getopt()` för avancerad argumenthantering: https://www.php.net/manual/en/function.getopt.php
- En guide till att skriva kommandoradsskript i PHP: https://www.phparch.com/2019/03/writing-command-line-scripts-with-php/
