---
date: 2024-01-26 03:50:37.644178-07:00
description: "PHP levereras med en interaktiv debugger som kallas Xdebug. S\xE5 h\xE4\
  r anv\xE4nder du den. F\xF6rst, se till att du har Xdebug installerat och konfigurerat\
  \ i din\u2026"
lastmod: '2024-03-13T22:44:38.000750-06:00'
model: gpt-4-0125-preview
summary: PHP levereras med en interaktiv debugger som kallas Xdebug.
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur man gör:
PHP levereras med en interaktiv debugger som kallas Xdebug. Så här använder du den.

Först, se till att du har Xdebug installerat och konfigurerat i din `php.ini`-fil:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Nästa, skriv ett enkelt PHP-skript med en bugg:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Hoppsan! Detta borde vara ett plus, inte ett minus
}

$resultat = add(1, 2);
echo "Resultatet är: $resultat"; // Utmatningen borde vara 3, inte -1
```

När du använder en IDE som PhpStorm, sätt en brytpunkt genom att klicka bredvid radnumret. Kör debuggern och se hur variablerna ändras när du stegar igenom exekveringen. När du stegar över `add`-funktionen kommer du att märka att `$resultat` blir -1, vilket är oväntat.

## Djupdykning:
Historiskt sett användes PHP främst för små skript, och felsökning var en fråga om att lägga till `var_dump()` och `print_r()`-uttalanden genom koden. Över tid, med PHP som blir en nyckelspelare i webbutveckling, började man använda mer sofistikerade verktyg som Xdebug och Zend Debugger.

Alternativ till Xdebug inkluderar pcov och phpdbg. Dessa erbjuder olika funktioner men kanske inte är lika fullständigt utrustade som Xdebug. phpdbg är en lättviktig, PHP-specifik debugger som distribueras med PHP sedan version 5.6, och pcov är en kodtäckningsdrivrutin.

När du implementerar en debugger, kom ihåg att du aldrig ska lämna debuggern påslagen på din produktionsserver, eftersom det kan exponera säkerhetsbrister och sänka prestandan.

## Se också:
- [Xdebug-dokumentation](https://xdebug.org/docs/)
- [PhpStorms felsökningsguide](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net om phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov på GitHub](https://github.com/krakjoe/pcov)
