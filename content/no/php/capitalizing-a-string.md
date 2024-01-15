---
title:                "Stor bokstav fra streng"
html_title:           "PHP: Stor bokstav fra streng"
simple_title:         "Stor bokstav fra streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å øke første bokstav i en streng kan være nyttig når man ønsker å formatere data eller gjøre den mer lesbar for brukeren. Det kan også være nødvendig i visse tilfeller for å kunne søke og sammenligne tekst på en mer nøyaktig måte.

## Slik gjør du det

Det finnes flere måter å kapitalisere en streng på i PHP. Her er to vanlige metoder som kan brukes:

```PHP
<?php
// Metode 1: Bruk ucwords() for å kapitalisere første bokstav i hvert ord i en streng
$streng = "dette er en tekststreng som skal kapitaliseres";
$kapitalisert_streng = ucwords($streng);
echo $kapitalisert_streng; // Output: Dette Er En Tekststreng Som Skal Kapitaliseres

// Metode 2: Bruk ucfirst() for å kun kapitalisere første bokstav i en streng
$streng = "dette er en tekststreng som skal kapitaliseres";
$kapitalisert_streng = ucfirst($streng);
echo $kapitalisert_streng; // Output: Dette er en tekststreng som skal kapitaliseres
```

Som du kan se, vil begge metodene gi ulikt resultat i de to siste ordene i strengen. Det er derfor viktig å velge riktig metode basert på hva som er ønsket resultat.

## Dypdykk

I PHP er begge disse funksjonene avhengig av språkinnstillingene for å kunne virke ordentlig. Dersom du ønsker å kapitalisere en streng uavhengig av språkinnstillingene, kan du bruke ucfirst() istedenfor. 

Å kapitalisere en streng kan også være nyttig når man jobber med databaser eller filbehandling. For eksempel kan man bruke disse funksjonene til å formatere data før de blir lagret i en database, eller til å gjøre data mer lesbar når man leser fra en fil.

## Se også

- [ucwords() dokumentasjon på PHP.net](https://www.php.net/manual/en/function.ucwords.php)
- [ucfirst() dokumentasjon på PHP.net](https://www.php.net/manual/en/function.ucfirst.php)