---
title:                "PHP: Ekstrahering av delstrenger"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Et av de mest grunnleggende elementene i strengmanipulering i PHP er å kunne trekke ut deler av en streng, også kjent som substringer. Dette er en viktig ferdighet for å kunne behandle data og utføre forskjellige operasjoner på det. Uansett om du er en erfaren PHP-utvikler eller en nybegynner, er å kunne ekstrahere substringer noe som vil være svært nyttig i mange ulike situasjoner.

## Hvordan

For å trekke ut en substring i PHP, bruker du funksjonen `substr()`. Denne funksjonen tar tre parametere: den opprinnelige strengen, startposisjonen for den ønskede substringen og lengden på substringen. La oss se på et enkelt eksempel:

```PHP
$streng = "Hei, dette er en streng.";
$substring = substr($streng, 4, 4);

echo $substring;
```

Outputen vil være "dett", siden startsposisjonen er 4 og lengden på substringen er 4. Du kan også bruke negative tall for startposisjon og lengde. Dette vil da telle bakfra i strengen.

## Dype granskninger

I tillegg til å bruke `substr()`-funksjonen, kan du også bruke andre funksjoner for å arbeide med substringer i PHP. For eksempel kan du bruke `strpos()` for å finne posisjonen til en bestemt streng i en annen streng, og deretter bruke `substr()` for å trekke ut en substring fra den posisjonen. Det finnes også nyttige funksjoner som `str_replace()` for å erstatte deler av en streng og `str_split()` for å dele en streng inn i en array.

## Se også

- [PHP.net - Substr](https://www.php.net/manual/en/function.substr.php)
- [W3Schools - PHP substring functions](https://www.w3schools.com/php/php_ref_string.asp)