---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/php/printing-debug-output/
date:                  2024-01-20T17:53:00.335081-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Utskrift for feilsøking er som å droppe brødsmuler for å finne veien hjem; det hjelper utviklere å spore appens flyt og finne bugs. Vi gjør det fordi det gir innsikt som leder til raske reparasjoner.

## How to:
La oss raskt skrive ut variabelverdier med enkle PHP-funksjoner:

```PHP
<?php
$variabel = 'Hello, Norway!';
echo $variabel; // Skriv ut direkte til output

// Sjekk verdier med print_r() for arrays
$array = ['Vikings', 'Fjords', 'Midnight Sun'];
print_r($array);

// For en mer innsiktsfull debug, bruk var_dump()
var_dump($array);
?>
```

Eksempel på output:
```
Hello, Norway!
Array
(
    [0] => Vikings
    [1] => Fjords
    [2] => Midnight Sun
)
array(3) {
  [0]=>
  string(7) "Vikings"
  [1]=>
  string(6) "Fjords"
  [2]=>
  string(12) "Midnight Sun"
}
```

## Deep Dive:
Før `print_r()` og `var_dump()`, utviklere måtte skrive egne funksjoner for å spore variabler. I dag bruker noen også `xdebug`, en PHP-utvidelse som forbedrer feilsøking ved å tilby stacksporing og avanserte breakpoints.

Her er et lite innsyn i implementasjonsdetaljer:
- `echo` er en språkkonstruksjon, ikke en funksjon, så den er marginalt raskere.
- `print_r()` er kjekk for lesbar utskrift av arrays og objekter, men viser ikke typer eller lengder.
- `var_dump()` derimot, er mer detaljert og nyttig når type eller lengde av verdier er relevante for feilen.

For ikke-invasiv feilsøking brukes ofte logging til en fil eller en konsoll via `error_log()`, som lar deg beholde logs selv etter at problemet er løst.

## See Also:
Her er noen ressurser for videre utforskning:

- PHPs offisielle dokumentasjon om strings:
  https://www.php.net/manual/en/language.types.string.php
- Et dypdykk i Xdebug for PHP:
  https://xdebug.org/docs
- PHP-feillogging:
  https://www.php.net/manual/en/function.error-log.php
