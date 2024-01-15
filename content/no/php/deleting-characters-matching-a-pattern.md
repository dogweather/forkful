---
title:                "Slette tegn som matcher et mønster"
html_title:           "PHP: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å slette tegn som samsvarer med et gitt mønster i en PHP-kode. For eksempel, hvis du har en tekststreng som inneholder uønsket formatering eller spesielle tegn, kan du bruke dette verktøyet til å rydde opp og få ren tekst.

## Hvordan du gjør det

For å slette tegn som matcher et mønster i en PHP-kode, kan du bruke funksjonen `preg_replace()` og angi det ønskede mønsteret som det første argumentet og en tom streng som det andre argumentet. La oss si at vi har følgende tekststreng:

```PHP
$tekst = 'Dette er & en * tekst $ med % spesielle @ tegn!';
```

Hvis vi ønsker å slette alle tegn som ikke er bokstaver og tall, kan vi bruke følgende kode:

```PHP
$rens_vanntette = preg_replace('/[^a-zA-Z0-9]/', '', $tekst);
```

Det første argumentet `/[^a-zA-Z0-9]/` betyr at vi vil finne alle tegn som ikke er inkludert i bokstavene a-z og tallene 0-9. Det andre argumentet er en tom streng, som betyr at alle disse tegnene vil bli slettet. Koden vil derfor gi følgende utgang:

```PHP
Detteerentekstmedspesielletegn
```

Som du kan se, har alle spesialtegn blitt slettet fra teksten.

## Deep Dive

`preg_replace()`-funksjonen er en del av PHPs innebygde regular expressions-bibliotek. Regular expressions er en serie med spesielle tegn og symboler som brukes til å søke og manipulere tekststrenger. Det kan være ganske komplekst å forstå til å begynne med, men med litt øvelse og erfaring kan det være et kraftig verktøy for å behandle tekststrenger.

I eksempelet ovenfor brukte vi metakarakteret `[^` for å finne tegn som ikke er inkludert i et gitt sett. Det finnes også andre metakarakterer som hjelper til med å finne visse typer tegn. Noen av de vanligste er:

- `.`: matcher alle tegn
- `\d`: matcher et tall
- `\w`: matcher et alfanumerisk tegn (bokstav eller tall)
- `\s`: matcher et mellomrom

Disse metakarakterene kan også kombineres med andre spesialtegn og symboler for å lage mer komplekse mønstre. For å lære mer om dette, anbefales det å lese mer om regular expressions og eksperimentere med dem.

## Se også

- [PHP regex tutorial](https://www.php.net/manual/en/regexp.reference.php)
- [PHP preg_replace() function](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular expressions cheat sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)