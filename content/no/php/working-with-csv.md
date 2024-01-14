---
title:                "PHP: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV kan virke som en kjedelig og tidkrevende oppgave, men det er faktisk en svært nyttig ferdighet for enhver webutvikler. CSV (Comma Separated Values) er en enkel måte å lagre og utveksle store mengder data på, som ofte brukes til å overføre informasjon mellom ulike databaser eller programmer. Ved å beherske CSV-behandling vil du kunne effektivisere arbeidet ditt og få mer tid til å fokusere på andre oppgaver.

## Hvordan

PHP har innebygd støtte for å jobbe med CSV-filer, som gjør det enkelt å lese og skrive data fra og til disse filene. La oss se på et eksempel hvor vi ønsker å lese data fra en CSV-fil og skrive det ut på nettsiden vår:

```PHP
<?php
// Åpne CSV-filen for lesing
$handle = fopen("data.csv", "r");

// Løkke som går gjennom hver rad i filen
while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
    // Skriv ut dataen på skjermen
    echo $data[0] . " | " . $data[1] . "<br>";
}

// Lukk filen
fclose($handle);
?>
```

La oss si at vi har en CSV-fil med følgende data:

```
Navn,Alder
Per,25
Kari,28
Ole,30
```

Koden vår vil da skrive ut følgende på nettsiden:

```
Per | 25
Kari | 28
Ole | 30
```

Nå som vi har lært hvordan vi kan lese og skrive data til og fra CSV-filer, kan vi også gjøre mer avanserte operasjoner som å sortere data eller filtrere ut bestemte rader. Dette er bare noen få eksempler på hva som er mulig med CSV-behandling i PHP.

## Dypdykk

For de som ønsker å lære mer om å jobbe med CSV i PHP, er det flere ressurser tilgjengelig på nettet. PHPs offisielle dokumentasjon inneholder detaljert informasjon om funksjoner og metoder for å behandle CSV-data. Det finnes også mange artikler og tutorials som kan hjelpe deg å bli mer komfortabel med å arbeide med denne typen data.

En viktig ting å huske på når du jobber med CSV-filer er å sørge for at dataene er formatert på riktig måte. For eksempel må du være oppmerksom på om filen bruker komma, semikolon eller et annet tegn for å skille dataene.

## Se også

- [Offisiell PHP-dokumentasjon om CSV](https://www.php.net/manual/en/book.csv.php)
- [Tutorial: Working with CSV and Excel in PHP](https://www.codexworld.com/export-html-table-data-to-excel-csv-png-php/)
- [How to Handle CSV in PHP - A Beginner's Guide](https://www.hostinger.com/tutorials/how-to-handle-csv-in-php-guide)