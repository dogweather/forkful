---
title:    "PHP: Konvertering av dato til streng"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Om du noen gang har jobbet med datoer i PHP, har du kanskje opplevd behovet for å konvertere en dato til en streng. Dette kan være nyttig for å vise datoer i et spesifikt format, eller for å lagre datoer i en database. Uansett formål, så er konvertering av dato til streng et viktig konsept i PHP som det er verdt å lære seg.

## Slik gjør du det

Det finnes flere måter å konvertere en dato til en streng i PHP. Her er to enkle eksempler:

```PHP
$today = strtotime("today"); // Returnerer dagens dato
echo date("d.m.Y", $today); // Output: 04.11.2020

$date = strtotime("25th of December 2020"); // Returnerer dato for 25. desember 2020
echo date("l, F jS, Y", $date); // Output: Friday, December 25th, 2020
```

La oss bryte ned det første eksempelet. Først bruker vi PHP-funksjonen `strtotime()` for å konvertere teksten "today" til en Unix-tidsstempel. Deretter bruker vi `date()` for å formatere denne datoen til ønsket format, i dette tilfellet for å vise dag, måned og år.

I det andre eksempelet bruker vi `strtotime()` igjen, men denne gangen med en annen tekst som angir en bestemt dato. Vi gjør så det samme som i det første eksempelet og bruker `date()` for å formatere datoen i ønsket form.

En annen måte å konvertere en dato til streng på er å bruke `DateTime`-klassen i PHP, som gir mer fleksibilitet for å håndtere datoer. Her er et eksempel på å konvertere en dato ved hjelp av `DateTime`:

```PHP
$date = new DateTime("2020-10-15"); // Oppretter en DateTime-objekt med dato 15. oktober 2020
echo $date->format("Y-m-d"); // Output: 2020-10-15
```

## Dykk ned i det

Når du konverterer en dato til en streng, er det viktig å være klar over forskjellige formateringsalternativer som er tilgjengelige i PHP. `date()`-funksjonen støtter en rekke formateringskoder som du kan bruke til å tilpasse datoformatet ditt. For en fullstendig liste over disse kodene, kan du se dokumentasjonen til PHP.

Når det gjelder å bruke `DateTime`-klassen, kan du også bruke forskjellige metoder for å få tak i spesifikke deler av datoen, for eksempel år, måned og dag. Dette gjør det enklere å manipulere datoer og utføre forskjellige operasjoner med dem.

En annen ting å huske på er tidszoner. Datoer kan være påvirket av tidszonen du er i, så det er viktig å være klar over dette når du konverterer datoer til strenger og vice versa.

## Se også

- [PHP-dokumentasjonen for date()](https://www.php.net/manual/en/function.date.php)
- [PHP-dokumentasjonen for DateTime](https://www.php.net/manual/en/class.datetime.php)
- [En guide til å håndtere datoer i PHP](https://www.tutorialrepublic.com/php-tutorial/php-date-and-time.php)