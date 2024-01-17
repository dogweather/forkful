---
title:                "Omdanne en dato til en tekststreng"
html_title:           "PHP: Omdanne en dato til en tekststreng"
simple_title:         "Omdanne en dato til en tekststreng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av datoer til strenger er en vanlig oppgave for PHP-programmer. Dette refererer til å endre et datoobjekt til en tekstlig representasjon, for eksempel for å vise datoen på en nettside eller i en e-post. Dette er nyttig for å gjøre datoen mer lesbar for brukere, og for å oppfylle spesifikke formateringskrav.

## Slik Gjør Du:
Her er et eksempel på hvordan du konverterer en date til streng i PHP:
```PHP
$date = new DateTime('2019-06-15');
echo $date->format('d-m-Y'); // output: 15-06-2019
```
I dette eksempelet bruker vi DateTime-klassen til å opprette et datoobjekt med en gitt dato. Vi bruker deretter "format" -funksjonen for å konvertere datoen til ønsket strengformat. I dette tilfellet har vi satt formatet til å være dag-måned-år. Det finnes mange forskjellige formateringsalternativer som du kan bruke, avhengig av dine behov.

## Dypdykk:
Historisk sett har konvertering av datoer til strenger vært en kompleks oppgave for programmerere. Før PHP 5.2 var hovedalternativet å bruke "strftime" -funksjonen, noe som krevde mye koding for å formatere datoer riktig. Med innføringen av DateTime-klassen i PHP 5.2 ble det mye enklere å konvertere datoer til strenger på en mer effektiv måte.

Selv om det er flere måter å gjøre dette på i PHP, er DateTime-klassen den anbefalte metoden. Alternativet "date" -funksjonen kan også brukes, men denne er mer begrenset i formateringsmuligheter og brukes ofte bare til eldre kode.

Når det gjelder implementeringsdetaljer, kan det være nyttig å sjekke ut PHPs offisielle dokumentasjon for å få en fullstendig liste over formateringsalternativer og andre relevante funksjoner.

## Se Også:
- [PHP DateTime-klassen](https://www.php.net/manual/en/class.datetime.php)
- [PHP date-funksjonen](https://www.php.net/manual/en/function.date.php)
- [PHP strftime-funksjonen](https://www.php.net/manual/en/function.strftime.php)