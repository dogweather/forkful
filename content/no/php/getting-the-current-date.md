---
title:                "Hente nåværende dato"
html_title:           "PHP: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor
Å få den nåværende datoen er en viktig del av dataprogrammering. Det tillater programmerere å vise den aktuelle datoen til brukere eller bruke den til å beregne tidsforskjeller for å skape dynamiske funksjoner.

# Hvordan du gjør det
For å få den nåværende datoen i PHP, kan du bruke funksjonen `date()`. Denne funksjonen tar inn et formatparameter som bestemmer hvordan datoen skal vises. La oss se på noen eksempler:

```
<?php
// Viser datoen i standardformat
echo date("d.m.Y"); //Output: 05.09.2021

// Viser datoen i dag/måned/år format
echo date("j/n/Y"); //Output: 5/9/2021

// Viser datoen med navnet på måneden
// på norsk
echo date("j. F Y"); //Output: 5. september 2021
?>
```

En annen nyttig funksjon for å hente inn den nåværende datoen er `strtotime()`. Denne funksjonen tar inn en tekststreng som angir en dato og konverterer den til et dato-objekt. La oss se på et eksempel:

```
<?php
// Konverterer en tekststreng til en dato
$dato = strtotime("next Sunday");

// Viser datoen i dag/måned/år format
echo date("j/n/Y", $dato); //Output: 12/9/2021
?>
```

Det er også mulig å få tak i den nåværende datoen i et bestemt tidsformat ved å bruke `time()`-funksjonen. Denne funksjonen returnerer antall sekunder som har gått siden 1. januar 1970, og kan dermed brukes til å lage timer, minutter og sekunder.

# Dykk ned i det
PHP har et stort utvalg av funksjoner for å håndtere datoer og tid. Det er verdt å ta seg tid til å lære seg de ulike funksjonene og formatene som er tilgjengelige for å få den nåværende datoen. Dette vil være nyttig når du jobber med komplekse prosjekter som krever nøyaktig tidsstyring.

Noen av de andre nyttige funksjonene i PHP for å håndtere datoer og tid inkluderer `date_default_timezone_set()` som lar deg sette tidssonen for din applikasjon, og `strtotime()` som kan konvertere en tekststreng til en dato.

# Se også
- [PHP Manual - Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [W3Schools - PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [TutorialsPoint - PHP Date and Time](https://www.tutorialspoint.com/php/php_date_time.htm)