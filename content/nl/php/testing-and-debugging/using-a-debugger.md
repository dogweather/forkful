---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.457969-07:00
description: "Hoe te gebruiken: PHP wordt geleverd met een interactieve debugger genaamd\
  \ Xdebug. Zo gebruik je het. Eerst, zorg ervoor dat je Xdebug hebt ge\xEFnstalleerd\u2026"
lastmod: '2024-03-13T22:44:50.899950-06:00'
model: gpt-4-0125-preview
summary: PHP wordt geleverd met een interactieve debugger genaamd Xdebug.
title: Een debugger gebruiken
weight: 35
---

## Hoe te gebruiken:
PHP wordt geleverd met een interactieve debugger genaamd Xdebug. Zo gebruik je het.

Eerst, zorg ervoor dat je Xdebug hebt geïnstalleerd en geconfigureerd in je `php.ini` bestand:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Vervolgens, schrijf een eenvoudig PHP-script met een bug:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Oeps! Dit zou een plus moeten zijn, geen min
}

$resultaat = add(1, 2);
echo "Resultaat is: $resultaat"; // Uitvoer zou 3 moeten zijn, niet -1
```

Gebruik een IDE zoals PhpStorm, zet een breekpunt door naast het regelnummer te klikken. Voer de debugger uit en kijk hoe de variabelen veranderen terwijl je door de uitvoering stapt. Wanneer je over de `add` functie stapt, zul je merken dat `$resultaat` -1 wordt, wat onverwacht is.

## Diepe Duik:
Historisch gezien werd PHP voornamelijk gebruikt voor kleine scripts, en debuggen was een kwestie van `var_dump()` en `print_r()` statements door de code heen toevoegen. In de loop van de tijd, met PHP die een sleutelrol speelt in webontwikkeling, kwamen er meer geavanceerde hulpmiddelen zoals Xdebug en Zend Debugger in gebruik.

Alternatieven voor Xdebug zijn onder andere pcov en phpdbg. Deze bieden verschillende functies, maar zijn mogelijk niet zo compleet als Xdebug. phpdbg is een lichtgewicht, PHP-specifieke debugger die sinds PHP 5.6 met PHP wordt meegeleverd, en pcov is een code-coverage-stuurprogramma.

Wanneer je een debugger implementeert, onthoud dan dat je de debugger nooit ingeschakeld moet laten op je productieserver, aangezien dit beveiligingsproblemen kan blootleggen en de prestaties kan vertragen.

## Zie Ook:
- [Xdebug Documentatie](https://xdebug.org/docs/)
- [PhpStorm Debugging Gids](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net over phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov op GitHub](https://github.com/krakjoe/pcov)
