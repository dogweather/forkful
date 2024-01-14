---
title:                "PHP: Konvertering av dato til streng"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en dato til en streng er et viktig konsept innen PHP-programmering. Det lar deg vise datoen i et bestemt format for å kunne bruke den i forskjellige situasjoner, som å vise datoen i en lesbar form i en utskrift eller å lagre den i en database. Uten å konvertere datoen til en streng, ville det være vanskelig å arbeide med den i koden din.

# Hvordan

For å konvertere en dato til en streng i PHP, kan du bruke funksjonen `date()`. Denne funksjonen tar to parametre: et format for hvordan du vil vise datoen, og datoen du vil konvertere. La oss si at vi vil konvertere dagens dato til det vanlige norske formatet DD.MM.YYYY. Vi kan gjøre dette ved å skrive følgende i en PHP-fil:

```PHP
<?php
echo date("d.m.Y");
?> 
```

Dette vil gi oss utskriften "14.10.2020". Vi kan også legge til en variabel eller en konstant for å konvertere en spesifikk dato, for eksempel:

```PHP
<?php
$birthday = "1977-01-31";
echo date("d.m.Y", strtotime($birthday));
?> 
```

Dette vil gi oss utskriften "31.01.1977". Vi brukte også funksjonen `strtotime()` for å konvertere datostrengen til et tallformat som `date()`-funksjonen kan forstå.

# Dypdykk

Det er viktig å merke seg at `date()`-funksjonen vil konvertere datoen basert på serverens tidssoneinnstillinger. Hvis du vil konvertere datoen basert på en annen tidssone, kan du bruke funksjonen `date_default_timezone_set()` før du kaller `date()`-funksjonen.

Det finnes også flere konverteringstegn som du kan bruke i `date()`-funksjonen for å tilpasse formatet til datoen din. Du kan se en fullstendig liste over disse konverteringstegnene på PHPs offisielle nettside.

# Se også

* [PHP date() funksjonen dokumentasjon](https://www.php.net/manual/en/function.date.php)
* [w3schools - PHP date() funksjonen](https://www.w3schools.com/php/func_date_date.asp)
* [Tutorialedge - Converting a date to a string in PHP](https://tutorialedge.net/php/converting-date-string-php/)