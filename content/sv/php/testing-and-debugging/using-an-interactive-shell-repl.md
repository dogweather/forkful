---
date: 2024-01-26 04:17:05.962961-07:00
description: "Hur man g\xF6r: Starta PHP REPL genom att k\xF6ra `php -a` i din terminal.\
  \ H\xE4r \xE4r ett smakprov p\xE5 hur det fungerar."
lastmod: '2024-03-13T22:44:37.997902-06:00'
model: gpt-4-0125-preview
summary: "Starta PHP REPL genom att k\xF6ra `php -a` i din terminal."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
Starta PHP REPL genom att köra `php -a` i din terminal. Här är ett smakprov på hur det fungerar:

```php
php > echo "Hej, världen!";
Hej, världen!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Du kan också definiera funktioner:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Fördjupning
REPLs har funnits i någon form sedan de tidiga dagarna av LISP på 1960-talet. PHP:s interaktiva skal är mindre avancerat jämfört med de i språk som Python eller JavaScript. Det behåller inte tillstånd mellan sessioner och saknar funktioner som autokomplettering. För en mer funktionsrik PHP REPL, överväg alternativ som `psysh` eller `boris`. Dessa tredjepartsskal erbjuder bättre verktyg för introspektion, flikkomplettering och till och med en debugger.

Under skalet fungerar PHP:s REPL genom att kompilera och exekvera varje rad kod som matas in. Begränsningarna med detta tillvägagångssätt blir tydliga med saker som att återdeklarera klasser, vilket inte är möjligt i samma session. Det är utmärkt för enkla tester men kan bli besvärligt för komplexa uppgifter.

## Se även
- [PHP-manualen - Interaktivt skal](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: En körningstid utvecklarkonsol, interaktiv debugger och REPL för PHP](https://psysh.org/)
- [Boris: En liten REPL för PHP](https://github.com/borisrepl/boris)
