---
title:                "PHP: Søking og erstatning av tekst"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver kode, er det ofte behov for å endre deler av teksten. Dette kan være å erstatte ord eller endre variabler. I slike tilfeller er det nyttig å kunne søke gjennom koden og enkelt bytte ut tekst. Dette kan spare deg for mye tid og gjøre kodingen mer effektiv.

## Hvordan

For å søke og erstatte tekst i PHP, kan man bruke funksjonen `str_replace()`. Denne funksjonen tar inn tre parametere: søketekst, erstattningstekst og en variabel som skal modifiseres. Her er et eksempel på hvordan man kan søke og erstatte i en tekststreng:

```PHP
$tekst = "Dette er en test";
$erstattning = "en praktisk løsning";
$søk = "test";
$modifisert = str_replace($søk, $erstattning, $tekst);
echo $modifisert; // Dette er en praktisk løsning
```

Som du kan se, bruker vi funksjonen til å bytte ut "test" med "en praktisk løsning" i teksten "Dette er en test". Det er også mulig å bruke funksjonen til å erstatte flere tekster samtidig:

```PHP
$tekst = "Dette er en test på søk og erstatning";
$erstattning = ["en praktisk løsning", "effektiv måte"];
$søk = ["test", "søk og erstatning"];
$modifisert = str_replace($søk, $erstattning, $tekst);
echo $modifisert; // Dette er en praktisk løsning på effektiv måte
```

I dette eksempelet erstatter vi både "test" og "søk og erstatning" med "en praktisk løsning" og "effektiv måte".

## Dypdykk

I tillegg til `str_replace()` funksjonen, har PHP også andre funksjoner som kan være nyttige for å søke og erstatte tekst. Noen av disse er:

- `str_ireplace()` som gjør det mulig å søke og erstatte uten å være opptatt av store og små bokstaver.
- `preg_replace()` som bruker regulære uttrykk for å søke og erstatte tekst.
- `strtr()` som gjør det mulig å bytte ut flere tegn samtidig.

Det er viktig å merke seg at søk og erstatning kan være en ressurskrevende operasjon, spesielt når det gjelder store tekstdokumenter. Derfor kan det være lurt å begrense bruken til tilfeller der det er nødvendig.

## Se også

- [PHP dokumentasjon om str_replace()](https://www.php.net/str_replace)
- [W3Schools tutorial om å søke og erstatte i PHP](https://www.w3schools.com/php/php_string.asp)