---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:18.215910-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve shell of REPL (Read-Eval-Print Loop) stelt je in staat om PHP-code ter plekke te schrijven en uit te voeren. Het is ideaal voor experimenteren, debuggen of leren, omdat je stukjes code kunt testen zonder de overhead van het creëren van een volledig script.

## Hoe te:
Start de PHP REPL door `php -a` in je terminal uit te voeren. Hier is een voorproefje van hoe het werkt:

```php
php > echo "Hallo, Wereld!";
Hallo, Wereld!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Je kunt ook functies definiëren:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Diepgaand
REPLs bestaan in een of andere vorm sinds de vroege dagen van LISP in de jaren 60. PHP's interactieve shell is minder geavanceerd in vergelijking met die van talen zoals Python of JavaScript. Het bewaart geen staat tussen sessies en mist functies zoals automatische aanvulling. Voor een feature-rijkere PHP REPL, overweeg alternatieven zoals `psysh` of `boris`. Deze shells van derden bieden betere introspectie-tools, tab-aanvulling, en zelfs een debugger.

Onder de motorkap werkt PHP's REPL door elke ingevoerde regel code te compileren en uit te voeren. De beperkingen van deze aanpak worden duidelijk bij zaken zoals het herdeclareren van klassen, wat niet mogelijk is in dezelfde sessie. Het is geweldig voor eenvoudige tests, maar kan omslachtig worden voor complexe taken.

## Zie Ook
- [PHP Handleiding - Interactieve shell](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Een runtime ontwikkelaar console, interactieve debugger en REPL voor PHP](https://psysh.org/)
- [Boris: Een mini REPL voor PHP](https://github.com/borisrepl/boris)
