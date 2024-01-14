---
title:                "PHP: Skriver til standard feil"
simple_title:         "Skriver til standard feil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange situasjoner hvor det kan være nyttig å skrive til standard error i stedet for standard output når du koder i PHP. Dette kan hjelpe deg med å fange feil og problemer som kan oppstå under kjøring av koden din.

## Hvordan
For å skrive til standard error i PHP kan du bruke funksjonen `fwrite()` sammen med åpnet filressurs `STDERR`. Dette vil skrive ut teksten du sender til standard error i stedet for standard output. Her er et eksempel på hvordan du kan gjøre dette:

```PHP
<?php
$tekst = "Dette er en feilmelding.";
fwrite(STDERR, $tekst);
?>
```

Dette vil skrive ut teksten "Dette er en feilmelding." i den røde utdatastrømmen i stedet for den vanlige utdatastrømmen. Det vil også returnere `true` for å indikere at teksten ble skrevet til standard error.

## Dypdykk
Å skrive til standard error kan være spesielt nyttig når du jobber med komplekse applikasjoner eller feilsøker problemer i koden din. Ved å skrive til standard error, kan du få en mer detaljert utdata som kan hjelpe deg med å identifisere og løse feil.

Et annet triks du kan bruke er å kombinere skriving til standard error med `error_log()` funksjonen. Dette lar deg også skrive feilmeldinger og andre viktige meldinger til en loggfil. Dette kan være svært nyttig når du prøver å feilsøke problemer i produksjonsmiljøet.

## Se også
- [PHP error handling](https://www.php.net/manual/en/book.errorfunc.php)
- [Writing to standard error in PHP](https://www.php.net/manual/en/function.fwrite.php)
- [Using error_log() in PHP](https://www.php.net/manual/en/function.error-log.php)