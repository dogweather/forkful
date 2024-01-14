---
title:    "PHP: Å finne lengden til en streng"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden av en streng er en viktig ferdighet for enhver PHP-programmerer. Dette gjør det mulig å håndtere og manipulere tekst på en effektiv måte. Det er også nyttig for å validere brukerinput og behandle data fra ulike databaser. Uansett hva slags prosjekt du jobber med, er det sannsynligvis at du vil støte på en situasjon hvor du må finne lengden til en streng.

## Slik gjør du det

Det finnes flere måter å finne lengden av en streng på i PHP, men den enkleste og mest effektive metoden er å bruke funksjonen `strlen()`. Denne funksjonen tar en streng som argument og returnerer antall tegn i strengen.

````PHP
<?php
$string = "Hei alle sammen!";
echo strlen($string); // Output: 15
?>
````

Som du kan se, er det veldig enkelt å bruke `strlen()`-funksjonen. Det er viktig å merke seg at denne funksjonen teller antall tegn, ikke antall ord. Hvis du vil telle antall ord i en streng, kan du bruke funksjonen `str_word_count()`, som også tar en streng som argument.

````PHP
<?php
$string = "Hei alle sammen!";
echo str_word_count($string); // Output: 3
?>
````

Dette er spesielt nyttig hvis du trenger å begrense antall ord som en bruker kan skrive inn i et skjema eller visningsfelt.

## Dykk ned i detaljene

Det er viktig å merke seg at `strlen()`-funksjonen teller antall tegn, ikke antall bytes. Dette kan være et problem hvis du jobber med flerspråklige tekststrenger som inneholder tegn fra andre språk. I slike tilfeller kan det være nødvendig å bruke en annen metode for å finne lengden på strengen. En av disse metodene er `mb_strlen()`, som tar hensyn til multibyte-tegn i strengen.

Et annet aspekt å merke seg er at når du jobber med strenger som inneholder HTML-tags, kan det hende du ønsker å utelate disse taggene fra lengden som blir returnert. For å gjøre dette kan du bruke `strip_tags()`-funksjonen før du teller antall tegn i strengen.

## Se også

- [PHP.net - strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP.net - str_word_count()](https://www.php.net/manual/en/function.str-word-count.php)
- [PHP.net - mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP.net - strip_tags()](https://www.php.net/manual/en/function.strip-tags.php)