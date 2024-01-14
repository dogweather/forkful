---
title:    "PHP: Søking og erstatning av tekst"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

I programmering er det ofte nødvendig å endre tekststrenger i koden vår. Dette kan være for å rette skrivefeil, endre variabler eller implementere språklokalisering. I disse tilfellene kan verktøy for å søke og erstatte tekst være svært nyttige. I denne bloggposten vil vi dykke inn i hvordan man kan bruke PHP for å utføre disse handlingene.

## Hvordan

For å søke og erstatte tekst i PHP, kan vi bruke funksjonen `str_replace()`. Denne funksjonen tar inn tre argumenter: teksten du vil bytte ut, teksten du vil bytte til, og deretter variablene eller arrayene du ønsker å søke i. La oss se på et eksempel:

```php
$original_text = "Jeg elsker å programmere!";
$new_text = str_replace("elsker", "liker", $original_text);
echo $new_text;
```

I dette tilfellet erstatter vi ordet "elsker" med "liker" og får følgende utskrift:

```php
Jeg liker å programmere!
```

Vi kan også bruke `str_replace()` til å erstatte flere forekomster av samme tekst, eller til og med bytte ut deler av en tekststreng basert på et mønster. Det finnes også andre funksjoner i PHP som kan hjelpe med mer avansert søking og erstatting, som for eksempel `preg_replace()`.

## Dypdykk

Det er viktig å merke seg at `str_replace()` er casesensitiv, noe som betyr at "elsker" og "Elsker" vil bli behandlet som to forskjellige ord. Dette kan føre til uønskede resultater hvis man ikke er oppmerksom på det. Vi kan også bruke `str_ireplace()` hvis vi ønsker å utføre en casesensitiv søking og erstatting.

I tillegg til å bruke disse funksjonene, kan vi også bruke regex-mønstre for å søke og erstatte tekst på en mer sofistikert måte. Dette kan være nyttig hvis vi trenger å endre alle forekomster av et ord uavhengig av plassering og formatering.

## Se også

- [PHP str_replace() dokumentasjon](https://www.php.net/manual/en/function.str-replace.php)
- [PHP preg_replace() dokumentasjon](https://www.php.net/manual/en/function.preg-replace.php)
- [Regex tutorial (på norsk)](https://www.ntnu.no/wiki/display/inf1005/Introduksjon+til+regex)