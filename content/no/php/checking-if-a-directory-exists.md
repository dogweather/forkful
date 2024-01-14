---
title:    "PHP: Sjekke om en mappe eksisterer."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Det å sjekke om en mappe eksisterer kan være en viktig oppgave når man jobber med PHP-programmering. Dette kan være nyttig for å sikre seg at man har tilgang til nødvendige filer og data, og for å kunne håndtere eventuelle feilsituasjoner.

## Slik gjør du det

Sjekke om en mappe eksisterer kan gjøres ved å bruke PHP-funksjonen `file_exists()`. Denne funksjonen sjekker om en fil eller mappe eksisterer på serveren. Dersom den returnerer `true`, betyr det at objektet eksisterer, mens `false` betyr at objektet ikke eksisterer.

For å sjekke om en mappe eksisterer, kan man bruke følgende kode:

```PHP
<?php
if(file_exists('min_mappe')) {
    echo "Mappen finnes!";
} else {
    echo "Mappen eksisterer ikke.";
}
```

Dersom mappen `min_mappe` eksisterer, vil koden over resultere i utskriften "Mappen finnes!", mens hvis mappen ikke eksisterer, vil koden gi utskriften "Mappen eksisterer ikke.".

## Dykk dypere

Det finnes også andre funksjoner som kan brukes for å sjekke om en mappe eksisterer, som for eksempel `is_dir()` og `is_writable()`. Begge disse funksjonene kan gi nyttig informasjon om mappen, som om den er en fil og om man har skriverettigheter til mappen.

Det er også verdt å merke seg at `file_exists()` fungerer for både lokale og eksterne filer og mapper, så lenge man har riktig sti og rettigheter. Det kan også være lurt å bruke absolutte stier istedenfor relative stier når man sjekker etter eksisterende filer og mapper.

## Se også

- [PHP sin offisielle dokumentasjon](https://www.php.net/manual/en/function.file-exists.php)
- [En guide til å sjekke fil- og mappeeksistens i PHP](https://www.geeksforgeeks.org/php-file_exists-vs-is_file-function/)
- [En artikkel om relativ og absolutt sti i PHP](https://www.w3schools.com/php/php_includes.asp)