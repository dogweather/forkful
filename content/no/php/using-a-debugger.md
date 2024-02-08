---
title:                "Å bruke en feilsøker"
aliases:
- no/php/using-a-debugger.md
date:                  2024-01-26T03:50:42.836384-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En debugger er et verktøy som hjelper programmerere med å forstå hva koden deres faktisk gjør mens den kjører. Det er forstørrelsesglasset som lar oss zoome inn på feil—de irriterende problemene som får programmene våre til å krasje eller spy ut feil svar—og knuse dem. Vi bruker debuggere fordi de sparer oss for timer med utskriftsuttrykk og gjetteleker.

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
