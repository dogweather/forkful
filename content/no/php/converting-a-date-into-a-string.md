---
title:                "Konvertere en dato til en streng"
html_title:           "PHP: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne artikkelen skal vi se på hvordan man kan konvertere en dato til en streng i PHP. Dette er en nyttig ferdighet for å kunne vise datoer i et forståelig format, for eksempel på en nettside eller i en applikasjon. 

## Slik gjør du det

Det finnes flere måter å konvertere en dato til en streng i PHP, avhengig av hva slags format du ønsker å vise den i. Her er noen eksempler: 

```PHP
// Konverter dato til vanlig leselig format
echo date("d.m.Y"); // Output: 12.05.2021

// Konverter dato til ISO-format
echo date("Y-m-d"); // Output: 2021-05-12

// Konverter dato til et mer spesifikt format
$myDate = strtotime("2021-05-12");
echo date("l, F jS Y", $myDate); // Output: Wednesday, May 12th 2021
```

Det finnes også en funksjon i PHP kalt `strftime()` som kan brukes til å konvertere datoer til tekst basert på et spesifisert format. For eksempel: 

```PHP
$myDate = strtotime("2021-05-12");
setlocale(LC_ALL, 'nb_NO'); // Setter locale til norsk for å få norsk format
echo strftime("%A %d. %B %Y", $myDate); // Output: onsdag 12. mai 2021
```

Som du ser, kan man spesifisere både hvilket språk og hvilket format man ønsker å bruke. Dette er spesielt nyttig for å tilpasse visningen av datoer til ulike land og språk. 

## Dykk dypere

Når man konverterer en dato til en streng, kan det være nyttig å vite om noen spesielle formateringskoder som kan brukes. De mest brukte er: 

- `d` – dag i måned (to sifre, med null foran hvis nødvendig)
- `m` – måned (to sifre, med null foran hvis nødvendig)
- `Y` – år (fire sifre)
- `l` – dag i uken (helt utskrevet)
- `F` – månedsnavn (helt utskrevet)
- `S` – suffiks for dag (st, nd, rd, eller th)
- `A` – helt klokkeslett (am eller pm)
- `H` – time (24-timers format)
- `i` – minutter
- `s` – sekunder

Det finnes mange flere formateringsmuligheter, og du kan finne en komplett oversikt over dem på PHPs offisielle nettside. 

## Se også

- [date() function](https://www.php.net/manual/en/function.date.php)
- [strftime() function](https://www.php.net/manual/en/function.strftime.php)
- [List of formatting options for date() function](https://www.php.net/manual/en/datetime.format.php)