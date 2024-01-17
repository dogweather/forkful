---
title:                "Sammenligning av to datoer"
html_title:           "PHP: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Sammenligning av to datoer i programmering er en viktig oppgave for å sjekke og sammenligne tidsvinduer. Dette gjør det mulig å finne ut om en dato kommer før eller etter en annen, eller om de er like.

Programmerere gjør dette for å kunne sortere og filtrere data etter dato, for å lage dynamiske tidsbaserte funksjoner, og for å sjekke om en dato har passert eller ikke.

## Slik gjør du det:
For å sammenligne to datoer i PHP, kan du bruke funksjonen `strtotime ()` for å konvertere datoene til en tidsstempel, som er en numerisk representasjon av dato og tid. Deretter kan du bruke sammenligningsoperatørene (`<`, `>`, `==`) for å sammenligne de to tidsstemplene.

```PHP
$dato1 = "10.11.2019";
$dato2 = "15.10.2020";

$ts_dato1 = strtotime ($dato1);
$ts_dato2 = strtotime ($dato2);

if ($ts_dato1 < $ts_dato2) {
  echo "$dato1 kommer før $dato2";
} else if ($ts_dato1 > $ts_dato2) {
  echo "$dato2 kommer før $dato1";
} else {
  echo "$dato1 og $dato2 er like";
}

// Output: 10.11.2019 kommer før 15.10.2020
```

I eksemplet ovenfor blir datoene sammenlignet og den riktige setningen blir utskrevet basert på sammenligningsresultatet.

## Dykk ned:
I tidligere versjoner av PHP, som 4.3.0 og tidligere, ble funksjonen `mktime()` brukt til å sammenligne datoer. Denne funksjonen er nå foreldet og bør ikke lenger brukes.

Det finnes også et alternativ til å sammenligne tidsstempler ved å bruke funksjonen `date_diff()`, som beregner forskjellen mellom to datoer og returnerer den i ønsket format.

## Se også:
- [PHP date() funksjonen](https://www.php.net/manual/en/function.date.php) for å formatere datoer i PHP.
- [PHP time() funksjonen](https://www.php.net/manual/en/function.time.php) for å hente nåværende tidsstempel.
- [PHP strtotime() funksjonen](https://www.php.net/manual/en/function.strtotime.php) for å konvertere en dato til en tidsstempel.