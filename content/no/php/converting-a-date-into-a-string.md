---
title:    "PHP: Konvertering av en dato til en streng"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en viktig funksjon i PHP-programmering. Det lar deg vise datoer i et mer leselig format for brukere, samt manipulere datoer for å oppnå visse resultater. I denne bloggposten vil vi utforske hvordan du kan gjøre dette på en enkel måte.

## Hvordan

Å konvertere en dato til en streng i PHP er enkelt. Du må først opprette en variabel som inneholder en dato, enten ved å bruke `date()`-funksjonen eller ved å hente en dato fra en database eller et annet sted. Deretter bruker du `date_format()`-funksjonen til å formatere og konvertere datoen til en streng.

La oss se på et eksempel der vi konverterer dagens dato til en streng, og viser den i formatet "dd.mm.yyyy".

```PHP
$today = date("Y-m-d"); // oppretter en variabel med dagens dato
$today_string = date_format($today, "d/m/Y"); // konverterer datoen til en streng i ønsket format
echo $today_string; // viser strengen
```

Dette vil gi følgende output: 21/09/2021.

Du kan også bruke `strtotime()`-funksjonen til å konvertere en streng til en dato før du formaterer den. Dette kan være nyttig hvis du har en brukerinput eller en dato i et annet format enn standarden.

```PHP
$date_string = "10/09/2021"; // en dato i strengformat
$date = strtotime($date_string); // konverterer strengen til en dato
$date_formatted = date("Y-m-d", $date); // formaterer og konverterer datoen til ønsket format
echo $date_formatted; // viser datoen
```

Dette vil gi følgende output: 2021-09-10.

## Dypdykk

Hvis du ønsker å manipulere datoen før du konverterer den til en streng, kan du bruke `date_modify()`-funksjonen. Denne lar deg legge til eller trekke fra dager, måneder eller år fra en dato.

La oss se på et eksempel der vi legger til 7 dager til dagens dato og deretter konverterer den til en streng.

```PHP
$today = date("Y-m-d"); // oppretter en variabel med dagens dato
$date_modified = date_modify($today, "+7 days"); // legger til 7 dager til datoen
$date_string = date_format($date_modified, "d/m/Y"); // konverterer datoen til en streng
echo $date_string; // viser strengen
```

Dette vil gi følgende output: 28/04/2021.

Det finnes også mange andre funksjoner og metoder for å konvertere og formatere datoer i PHP, og det kan være lurt å sjekke dokumentasjonen for å finne det som passer best for ditt behov.

## Se også

- [PHP Date-funksjonen](https://www.php.net/manual/en/function.date.php)
- [PHP Date_format-funksjonen](https://www.php.net/manual/en/function.date-format.php)
- [PHP Strtotime-funksjonen](https://www.php.net/manual/en/function.strtotime.php)
- [PHP Date_modify-funksjonen](https://www.php.net/manual/en/function.date-modify.php)