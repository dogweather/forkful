---
title:    "PHP: Sammenligning av to datoer"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en viktig del av å utvikle en pålitelig og funksjonell nettside eller applikasjon. Dette kan hjelpe deg med å filtrere og sortere data etter dato i en database, lage kalendere eller tidsplaner, og mye mer.

## Slik gjør du det

For å sammenligne to datoer i PHP, kan du bruke funksjonen `strtotime ()`, som tar en dato som streng og konverterer den til et tall i sekunder siden 01.01.1970. Deretter kan du bruke dette tallet til å sammenligne to datoer og få ønsket resultat.

```PHP
$dato_en = "10. mai 2020";
$timestamp_en = strtotime($dato_en);

$dato_to = "15. mai 2020";
$timestamp_to = strtotime($dato_to);

if ($timestamp_to > $timestamp_en) {
    echo "$dato_to er senere enn $dato_en";
} elseif ($timestamp_to < $timestamp_en) {
    echo "$dato_to er tidligere enn $dato_en";
} else {
    echo "$dato_to og dato_en er like";
}
```

Dette eksemplet vil gi følgende output:

```
15. mai 2020 er senere enn 10. mai 2020
```

Du kan også bruke `date ()` funksjonen til å få en mer leselig form av datoene, noe som kan være nyttig for presentasjonen av data til brukere.

## Dykker dypere

Når man sammenligner datoer, er det viktig å være klar over eventuelle tidsforskjeller eller formater som kan påvirke resultatet. For eksempel kan en dato som er lagret i en database som UTC, bli konvertert til en annen tidssone når den blir hentet og deretter sammenlignet. Det er også viktig å være oppmerksom på skuddår og forskjellige måter å skrive datoer på, som for eksempel 05/10/2020 i stedet for 10. mai 2020.

For å unngå disse utfordringene, kan det være lurt å bruke en standard datoformat og konvertere alle datoer til dette formatet før du sammenligner dem. Du kan også bruke PHPs innebygde funksjoner som `date_create ()` og `date_diff ()` for å gjøre mer avanserte sammenligninger av datoer.

## Se også

- [PHP Manual: strtotime () funksjonen](https://www.php.net/manual/en/function.strtotime.php)
- [PHP Manual: date () funksjonen](https://www.php.net/manual/en/function.date.php)
- [PHP Manual: date_create () funksjonen](https://www.php.net/manual/en/function.date-create.php)
- [PHP Manual: date_diff () funksjonen](https://www.php.net/manual/en/function.date-diff.php)