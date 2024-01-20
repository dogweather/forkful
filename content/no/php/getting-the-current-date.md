---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente datoen handler om å spørre systemet ditt om hva dagens dato er. Denne infoen er uvurderlig når du jobber med ting som kalendere, dato-stempler eller hvis du bare vil si "Hei" til brukeren din med dagens dato.

## Slik gjør du det:

PHP gjør det enkelt å få dagens dato. Bruk `date()` funksjonen som følger:

```PHP
<?php
echo date('Y-m-d');
?>
```

Denne koden vil hente datoen i "År-Måned-Dag" format, og viser noe som dette: 

```
2023-06-23
```

## Dypdykk

`date()` funksjonen i PHP har vært til stede siden PHP 4, og har siden blitt utbedret og forbedret. Funksjonen bruker serverens dato og klokkeslett, noe som betyr at resultatet kan variere avhengig av hvor serveren er lokalisert - noe du bør ha i bakhodet. 

Det finnes alternativer til `date()`, som f.eks. `DateTime()` klassen som tilbyr mer fleksibilitet og funksjoner. Bruken av `DateTime()` kan se ut slik:

```PHP
<?php
$dato = new DateTime();
echo $dato->format('Y-m-d');
?>
```

Det er ikke så annerledes enn `date()` i bruk, men hvis du skal jobbe med mer komplekse datooperasjoner, vil `DateTime()` være det beste valget. 

## Se også 

For mer informasjon om å jobbe med datoer i PHP, ta en titt på følgende lenker:

1. PHP date() funksjonen: [https://www.php.net/manual/en/function.date.php](https://www.php.net/manual/en/function.date.php)
2. PHP DateTime klassen: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
3. Datoformat i PHP: [https://www.php.net/manual/en/datetime.format.php](https://www.php.net/manual/en/datetime.format.php)