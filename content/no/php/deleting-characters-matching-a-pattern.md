---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Slette tegn som samsvarer med et mønster i PHP er en måte å fjerne spesifikke karakterer fra en streng. Programmererne gjør dette for å rense input, validere data eller manipulere tekst på nyttige måter.

## Slik gjør du det:
```PHP
<?php

$tekst = "Hei, Velkommen til Norge!";
$mønster = '/[aeiou]/i';
$erstattet_tekst = preg_replace($mønster, '', $tekst);

echo $erstattet_tekst;
```
Når du kjører denne koden, vil outputtet bli:
```PHP
"H, Vlkmn tl Nrg!"
```
I dette eksemplet samsvarer vårt mønster med alle vokalene (både små og store bokstaver) i teksten og erstatter dem med en tom streng. 

## Dypdykk
Funksjonen `preg_replace()` som ble introdusert i PHP 4.0, er en del av PCRE (Perl Compatible Regular Expressions), en implementering av regular expressions syntaks. Denne metoden anses som mer kraftig og fleksibel enn andre alternativer som `str_replace()` fordi den kan håndtere komplekse mønstre.

Alternativt, i stedet for å slette, kan du også bruke funksjonen `preg_match()` eller `preg_match_all()` for å finne tegn som samsvarer med et mønster.

Husk på at `preg_replace()` fungerer ved å søke gjennom innledende strengen (`$tekst`), tegn for tegn, og sammenligne hvert tegn med mønsteret. Dette betyr at ytelsen kan reduseres med større strenger eller komplekse mønstre.

## Se også
For mer detaljert informasjon og dokumentasjon, se PHPs offisielle dokumentasjon på følgende lenker:

1. preg_replace() - [https://www.php.net/manual/en/function.preg-replace.php](https://www.php.net/manual/en/function.preg-replace.php)

2. preg_match() - [https://www.php.net/manual/en/function.preg-match.php](https://www.php.net/manual/en/function.preg-match.php)

3. PCRE - [https://www.php.net/manual/en/book.pcre.php](https://www.php.net/manual/en/book.pcre.php)