---
title:                "Debug-output afdrukken"
date:                  2024-01-28T22:04:27.262041-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het printen van debug-output gaat over het op het scherm knallen van de interne data van je code om uit te vogelen wat er aan de hand is. Programmeurs doen dit voor een realiteitscheck, om te zien waar de bugs zich verstoppen of gewoon om te zien of de code doet wat het moet doen.

## Hoe te:
```PHP
<?php
// Basis output
$variable = 'Debuggen is geweldig!';
echo $variable;

// Met print_r voor arrays
$myArray = ['appel', 'sinaasappel', 'banaan'];
echo '<pre>'; // Maakt het leesbaar
print_r($myArray);
echo '</pre>';

// var_dump voor details
$anotherArray = ['sleutel' => 'waarde', 'eenAndereSleutel' => 123];
var_dump($anotherArray);

// Naar het foutenlog
error_log('Dit gaat naar de logs voor meer onopvallende debugs.');
?>
```
Voorbeeld Output:
```
Debuggen is geweldig!
Array
(
    [0] => appel
    [1] => sinaasappel
    [2] => banaan
)
array(2) {
  ["sleutel"]=>
  string(5) "waarde"
  ["eenAndereSleutel"]=>
  int(123)
}
```

## Diepgaande Duik:
Debug-output is niet veel veranderd: het is er al sinds de vroege dagen toen oude programmeurs debugden met printf(). PHP heeft dit omarmd met `echo`, `print`, `print_r()` en `var_dump()`. Het is misschien niet fancy, maar het werkt. Moderne PHP-ontwikkelaars hebben ook Xdebug, dat stap voor stap door de code kan gaan en een chiquere output kan tonen. Voor logs heb je `error_log()`, dat berichten stiekem in serverlogs smokkelt zonder ze aan gebruikers bloot te stellen. Elk hulpmiddel heeft zijn plaats: `echo` en `print` zijn snel en vuil; `print_r()` is voor gebruiksvriendelijke inzichten in arrays; `var_dump()` geeft je de nitty-gritty over typen en lengtes; `error_log()` houdt dingen onder de radar als je in detective-modus bent op een live site.

## Zie Ook:
- De PHP handleiding over `echo`: https://www.php.net/manual/nl/function.echo.php
- Meer over `print_r()`: https://www.php.net/manual/nl/function.print-r.php
- De gritty details van `var_dump()`: https://www.php.net/manual/nl/function.var-dump.php
- Duik in het loggen met `error_log()`: https://www.php.net/manual/nl/function.error-log.php
- Xdebug, de beste vriend van de debugger: https://xdebug.org/docs/display
