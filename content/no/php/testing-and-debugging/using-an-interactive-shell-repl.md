---
date: 2024-01-26 04:16:24.036297-07:00
description: "Hvordan: Start PHP REPL ved \xE5 kj\xF8re `php -a` i terminalen din.\
  \ Her er en smakebit p\xE5 hvordan det fungerer."
lastmod: '2024-03-13T22:44:40.887994-06:00'
model: gpt-4-0125-preview
summary: "Start PHP REPL ved \xE5 kj\xF8re `php -a` i terminalen din."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
Start PHP REPL ved å kjøre `php -a` i terminalen din. Her er en smakebit på hvordan det fungerer:

```php
php > echo "Hallo, Verden!";
Hallo, Verden!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Du kan også definere funksjoner:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Dypdykk
REPL har eksistert i en eller annen form siden de tidlige dagene av LISP på 1960-tallet. PHP sin interaktive skall er mindre avansert sammenlignet med de i språk som Python eller JavaScript. Det lagrer ikke tilstand mellom økter og mangler funksjoner som autofullføring. For en mer funksjonsrik PHP REPL, vurder alternativer som `psysh` eller `boris`. Disse tredjepartsskallene tilbyr bedre introspeksjonsverktøy, tabulatorkompletering, og til og med en debugger.

Under hetten fungerer PHP sin REPL ved å kompilere og utføre hver linje med kode etter hvert som den blir inntastet. Begrensningene ved denne tilnærmingen blir klare med ting som å redefinere klasser, noe som ikke er mulig i samme økt. Det er flott for enkle tester, men kan bli tungvint for komplekse oppgaver.

## Se Også
- [PHP Manual - Interaktiv skall](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: En kjøretidsutviklerkonsoll, interaktiv debugger og REPL for PHP](https://psysh.org/)
- [Boris: En liten REPL for PHP](https://github.com/borisrepl/boris)
