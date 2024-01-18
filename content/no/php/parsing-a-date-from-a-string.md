---
title:                "Analysering av dato fra en streng"
html_title:           "PHP: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Parsing av datoer fra en streng er en vanlig oppgave for PHP-programmering. Det innebære å konvertere en streng (tekst) til en datoobjekt, slik at vi kan manipulere og bruke datoen i vårt script. Denne prosessen er nødvendig for å kunne håndtere og lagre datoer på en mer strukturert måte.

## Hvordan:

```PHP
// Eksempel 1:
$stringDate = "20/10/2021";
$date = date_create_from_format("d/m/Y", $stringDate);
echo date_format($date, "l, jS F Y"); // Utgang: Wednesday, 20th October 2021

// Eksempel 2:
$stringDate = "10-20-2021";
$date = date_create_from_format("m-d-Y", $stringDate);
echo date_format($date, "l, F jS, Y"); // Utgang: Wednesday, October 20th, 2021
```

## Dypdykk:

Parsing av datoer fra en streng er en viktig oppgave, spesielt i webutvikling. Dette skyldes ofte at data kommer inn som ustrukturerte tekster. PHP tilbyr en rekke funksjoner og metoder for å håndtere datoer, inkludert "date_create_from_format()" og "date_format()", som ble brukt i eksemplene ovenfor. Det finnes også flere alternative metoder for å parse datoer, som å bruke regex-mønster eller spesialiserte parser-biblioteker. Implementering av denne funksjonen kan variere avhengig av typen datoformat og koding av din applikasjon.

## Se også:

- PHP: [date_create_from_format()](https://www.php.net/manual/en/function.date-create-from-format.php)
- PHP: [date_format()](https://www.php.net/manual/en/function.date-format.php)
- W3Schools: [How to parse a date from a string in PHP](https://www.w3schools.com/php/php_date.asp)
- PHP.net: [Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)