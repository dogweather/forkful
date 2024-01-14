---
title:                "PHP: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger, når vi jobber med tekstbehandling, ønsker vi å behandle teksten på forskjellige måter avhengig av om bokstavene er store eller små. For eksempel, når vi lager et innloggingsystem, vil vi at brukerens passord skal være case-sensitive, men ikke brukernavnet. Det er her funksjonen for å konvertere en streng til små bokstaver kommer inn. Ved å konvertere all teksten til små bokstaver, vil vi unngå feil ved innlogging og andre sikkerhetsproblemer. 

## Hvordan

For å konvertere en streng til små bokstaver, bruker vi funksjonen `strtolower ()` i PHP. Her er et enkelt eksempel som viser frem hvordan dette fungerer:

```PHP
$originalStreng = "DEtte ER EN eksempelSTRiNG";
$nyStreng = strtolower($originalStreng);
echo $nyStreng;
```

Dette vil gi følgende utskrift:

```PHP
dette er en eksempelstring
```

Vi kan også bruke `strtolower ()` for å behandle hele tekststrenger som består av flere ord. Her er et eksempel på hvordan vi kan konvertere en hel setning til små bokstaver:

```PHP
$setning = "Hei, Dette ER En EKSEMPEL Setning";
$nySetning = strtolower($setning);
echo $nySetning;
```

Utskriften vil da være:

```PHP
hei, dette er en eksempel setning
```

Husk at `strtolower ()` funksjonen ikke bare konverterer store bokstaver til små, den vil også behandle alle spesialtegn og tall på samme måte.

## Dykk Dypere

Nå som vi har forstått hvordan man konverterer en streng til små bokstaver, la oss se på hvorfor dette er viktig i programmering. Som nevnt tidligere, er sikkerhet en viktig årsak til å bruke denne funksjonen. Ved å konvertere alle inntastede passord til små bokstaver, unngår vi mulige feil når brukeren logger seg inn.

Å bruke små bokstaver i alle tekststrenger kan også gjøre det enklere å sammenligne dem. For eksempel, hvis du vil sjekke om to strenger er like, kan det være lurt å konvertere dem til små bokstaver først for å unngå eventuelle forskjeller i store og små bokstaver.

## Se Også

- [PHP.net - strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [Tuttefel.com - Lowercase String with PHP](https://tuttefel.com/languages/php/lowercase-string-methods/)
- [W3Schools.com - PHP String Functions](https://www.w3schools.com/php/php_string_functions.asp)