---
title:                "Søking og erstatting av tekst"
html_title:           "PHP: Søking og erstatting av tekst"
simple_title:         "Søking og erstatting av tekst"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstatning av tekst er en vanlig oppgave for programmerere. Det er en måte å endre eller bytte ut tekst i en fil eller en streng med en annen tekst. Dette kan være nyttig når du ønsker å lage en batch endring eller rette opp feil i koden din.

## Slik gjør du det:
For å søke og erstatte tekst i PHP kan du bruke funksjonen `str_replace()`. Denne funksjonen tar tre parametere: strengen du ønsker å søke i, strengen du vil erstatte den med, og deretter selve teksten du ønsker å søke gjennom. For eksempel:

```PHP
$text = "Hei verden!";
echo str_replace("verden", "alle sammen", $text);
```

Dette vil gi følgende utskrift:

```PHP
Hei alle sammen!
```

Du kan også bruke `str_replace()` til å søke og erstatte tekst i en hel fil ved å lese inn filen som en streng og bruke funksjonen på denne strengen.

## Dykk dypere:
Søking og erstatning av tekst i programmering har vært en viktig oppgave siden begynnelsen av datamaskiner. I dag finnes det også andre metoder for å utføre dette, som for eksempel regulære uttrykk eller spesialiserte søkemotorer. Implementasjonen av `str_replace()` i PHP er basert på Unix-kommandoen `sed`.

## Se også:
- [PHP Manual](https://www.php.net/manual/en/function.str-replace.php)
- [W3Schools - PHP str_replace() Function](https://www.w3schools.com/php/func_string_str_replace.asp)
- [GeeksforGeeks - PHP str_replace() Function](https://www.geeksforgeeks.org/php-str_replace-function/)