---
title:                "PHP: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave for PHP-programmerere. Det kan være nyttig for å raskt gjøre endringer i store mengder tekst, som for eksempel i en database. Det sparer deg også tid og arbeid ved å gjøre endringer manuelt.

## Hvordan

For å søke og erstatte tekst i PHP, bruker vi innebygde funksjoner som `str_replace()` og `preg_replace()`. Disse funksjonene tar tre argumenter: teksten du ønsker å søke etter, teksten du ønsker å erstatte den med, og teksten du ønsker å søke i. Funksjonene kan også ta imot arrays av tekster for å søke etter flere uttrykk på en gang.

```PHP
// Søker og erstatter teksten "he" med "ha" i en string
$string = "Hello world!";
$ny_string = str_replace("he", "ha", $string);

echo $ny_string; // Output: Hallo world!

// Søker og erstatter teksten "a" og "e" med "X" i en string
$string = "Hello world!";
$sok = array("a", "e");
$erstatter = "X";
$ny_string = str_replace($sok, $erstatter, $string);

echo $ny_string; // Output: HXllo world!
```

For mer avansert søk og erstatning, kan vi bruke `preg_replace()` som tillater bruk av regulære uttrykk. Dette lar deg søke etter tekst basert på mønstre og utføre mer komplekse erstatninger.

```PHP
// Søker og erstatter alle tall i en string med "X"
$string = "12345abcde";
$ny_string = preg_replace("/[0-9]/", "X", $string);

echo $ny_string; // Output: XXXXXabcde
```

## Dypdykk

Det finnes også andre nyttige funksjoner for å søke og erstatte tekst i PHP, som for eksempel `str_ireplace()` som lar deg utføre en søk og erstatting uten å ta hensyn til store og små bokstaver, `strtr()` som gir deg muligheten til å erstatte flere tegn samtidig, og `substr_replace()` for å erstatte en del av en string basert på posisjon.

Det er også viktig å huske på at disse funksjonene kan være ganske ressurskrevende, spesielt hvis du bruker regulære uttrykk. Pass på å begrense søket ditt så mye som mulig for å øke hastigheten.

## Se også

- [PHP manual for str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP manual for preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP manual for strtr()](https://www.php.net/manual/en/function.strtr.php)
- [PHP manual for substr_replace()](https://www.php.net/manual/en/function.substr-replace.php)