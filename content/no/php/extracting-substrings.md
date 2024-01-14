---
title:    "PHP: Uthenting av delstrenger"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan hente ut deler av en tekststreng i PHP? Det kan være nyttig når du for eksempel skriver et program som skal behandle brukerinput eller formatere data. Ved å utnytte funksjoner for å trekke ut substrings, kan du gjøre koden din mer dynamisk og effektiv.

## Hvordan du gjør det

Å trekke ut substrings i PHP er enkelt med funksjonen `substr()`. Denne funksjonen tar imot tre parametere: den opprinnelige tekststrengen, startindeksen og antall tegn du vil hente ut. Her er et eksempel på hvordan du kan bruke `substr()`:

```PHP
$original = "Hei, jeg heter Andreas.";
$sub = substr($original, 5, 10);
echo $sub;
```

Outputen vil være "jeg heter".

## Dypdykk

Det finnes også flere andre funksjoner som kan brukes for å hente ut substrings i PHP. Blant annet `mb_substr()` som lar deg håndtere flerspråklige tekststrenger, og `str_split()` som lar deg splitte en tekststreng inn i en array av mindre tekststrenger.

Det kan også være nyttig å vite at du kan bruke negative tall som startindeks i `substr()` for å starte tellingen fra slutten av tekststrengen. For eksempel vil `substr($original, -5)` hente de siste fem tegnene i tekststrengen.

## Se også

- [PHP dokumentasjon for substr()](https://www.php.net/manual/en/function.substr.php)
- [W3Schools tutorial on substrings in PHP](https://www.w3schools.com/php/func_string_substr.asp)
- [Codecademy course on manipulating strings in PHP](https://www.codecademy.com/courses/learn-php/lessons/php-string-handling/exercises/substr)