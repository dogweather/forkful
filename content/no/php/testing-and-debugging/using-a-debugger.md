---
date: 2024-01-26 03:50:42.836384-07:00
description: "PHP kommer med en interaktiv debugger kalt Xdebug. Her er hvordan du\
  \ bruker den. F\xF8rst, s\xF8rg for at du har Xdebug installert og konfigurert i\
  \ din\u2026"
lastmod: '2024-03-13T22:44:40.891242-06:00'
model: gpt-4-0125-preview
summary: "PHP kommer med en interaktiv debugger kalt Xdebug. Her er hvordan du bruker\
  \ den. F\xF8rst, s\xF8rg for at du har Xdebug installert og konfigurert i din\u2026"
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
PHP kommer med en interaktiv debugger kalt Xdebug. Her er hvordan du bruker den.

Først, sørg for at du har Xdebug installert og konfigurert i din `php.ini`-fil:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Deretter, skriv et enkelt PHP-skript med en feil:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Ops! Dette skulle vært et pluss, ikke et minus
}

$resultat = add(1, 2);
echo "Resultatet er: $resultat"; // Utdata bør være 3, ikke -1
```

Ved å bruke et IDE som PhpStorm, sett et brytepunkt ved å klikke ved siden av linjenummeret. Kjør debuggeren og se hvordan variabler endres mens du stegvis gjennomfører utførelsen. Når du steg over `add`-funksjonen, vil du legge merke til at `$resultat` blir -1, noe som er uventet.

## Dypdykk:
Historisk sett ble PHP primært brukt for små skript, og debugging var et spørsmål om å legge til `var_dump()` og `print_r()`-uttrykk gjennom koden. Over tid, med PHP som en nøkkelspiller i webutvikling, kom mer sofistikerte verktøy som Xdebug og Zend Debugger i bruk.

Alternativer til Xdebug inkluderer pcov og phpdbg. Disse tilbyr ulike funksjoner, men er kanskje ikke så funksjonsrike som Xdebug. phpdbg er en lett, PHP-spesifikk debugger som er distribuert med PHP siden 5.6, og pcov er en kode dekningsdriver.

Når du implementerer en debugger, husk at du aldri bør la debuggeren være påslått på din produksjonsserver, da det kan eksponere sikkerhetsproblemer og redusere ytelsen.

## Se Også:
- [Xdebug Dokumentasjon](https://xdebug.org/docs/)
- [PhpStorm Debugging Guide](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net om phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov på GitHub](https://github.com/krakjoe/pcov)
