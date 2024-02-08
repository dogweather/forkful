---
title:                "Läsa in kommandoradsargument"
aliases:
- sv/php/reading-command-line-arguments.md
date:                  2024-01-20T17:56:38.322726-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument innebär att fånga text som skickas till ett PHP-script från terminalen. Programmerare gör detta för att tillåta interaktion och dynamiskt beteende i skripten.

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
