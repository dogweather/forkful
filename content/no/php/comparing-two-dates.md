---
title:                "PHP: Sammenligne to datoer"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt når man jobber med noe som involverer tid. Ved å sammenligne to datoer kan man finne ut om de er like, om den ene er før eller etter den andre, eller hvor lang tid det er mellom dem. Dette er nyttig for å kunne håndtere datoer på en effektiv måte i koden din. 

## Hvordan gjøre det
For å sammenligne to datoer i PHP, bruker vi funksjonen "strtotime()". Denne funksjonen tar inn en dato som en string, og konverterer den til et tidsstempel. Deretter kan vi sammenligne tidsstempelene på vanlig måte ved å bruke sammenligningsoperatorer som "==", "!=", ">=" osv. Her er et eksempel på å sammenligne to datoer og få ut en tekst basert på resultatet: 

```PHP
$firstDate = "2020-01-01";
$secondDate = "2020-02-01";

if(strtotime($firstDate) == strtotime($secondDate)) {
    echo "Datoene er like.";
} elseif(strtotime($firstDate) > strtotime($secondDate)) {
    echo "Første dato er etter andre dato.";
} else {
    echo "Første dato er før andre dato.";
}
```

Dette eksempelet vil gi følgende output: "Første dato er før andre dato."

## Dypdykk
Når vi sammenligner datoer i PHP, er det viktig å være klar over at funksjonen "strtotime()" ikke alltid er 100% nøyaktig. Det kan være tilfeller hvor den konverterer en dato feil, for eksempel hvis datoen er utenfor det gyldige datointervallet eller hvis den inneholder feil format. Derfor er det viktig å alltid sjekke og validere datoene før man sammenligner dem, for å unngå uforutsette resultater.

## Se også
- [PHP datofunksjoner](https://www.php.net/manual/en/ref.datetime.php)
- [Symfony DateComparator komponent](https://symfony.com/doc/current/components/expression_language/syntax.html#date-operators)
- [Laravel Carbon bibliotek](https://carbon.nesbot.com/docs/#api-comparison)