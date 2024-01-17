---
title:                "Kontrollere om en mappe eksisterer"
html_title:           "PHP: Kontrollere om en mappe eksisterer"
simple_title:         "Kontrollere om en mappe eksisterer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekking av om en mappe eksisterer er en svært nyttig funksjon for PHP-programmering. Det lar deg verifisere om en bestemt mappe eksisterer på serveren før du starter prosessen med å opprette nye filer eller strukturere data. På denne måten sparer du deg for unødvendig feilhåndtering og sørger for en jevnere kjøring av koden.

## Hvordan:
Her er et eksempel på hvordan du kan sjekke om en mappe eksisterer ved hjelp av PHP:
```PHP
$mappe = 'bildealbum';

// Sjekk om mappen eksisterer
if (file_exists($mappe)) {
    // Kode for handlinger hvis mappen eksisterer
    echo "Mappen finnes allerede!";
} else {
    // Kode for handlinger hvis mappen ikke eksisterer
    mkdir($mappe);
    echo "Mappen ble opprettet!";
}
```
Eksempelet sjekker om mappen "bildealbum" eksisterer, og hvis den ikke gjør det, opprettes den. Dette er en enkel og effektiv måte å sikre at koden din fungerer som den skal, uten å måtte håndtere feilsituasjoner.

## En dypdykk:
Sjekking av om en mappe eksisterer har vært en del av PHP siden versjon 4.0.2, og funksjonen er fortsatt tilgjengelig i dagens versjoner. Som et alternativ til å bruke "file_exists()" funksjonen, kan du også bruke "is_dir()" eller "glob()" funksjonene for å sjekke om en mappe eksisterer. Disse funksjonene tilbyr også mer avanserte muligheter for håndtering av mapper og filer.

En viktig ting å merke seg er at når du sjekker om en mappe eksisterer, så vil funksjonen fortsette å kjøre selv om mappen eksisterer. Det betyr at du kan bruke denne sjekken som en del av en handling, som å bare lage mappen hvis den ikke allerede finnes.

## Se også:
For mer informasjon om sjekking av mapper og filer i PHP, kan du sjekke ut PHPs offisielle dokumentasjon:
- [file_exists() dokumentasjon](https://www.php.net/manual/en/function.file-exists.php)
- [is_dir() dokumentasjon](https://www.php.net/manual/en/function.is-dir.php)
- [glob() dokumentasjon](https://www.php.net/manual/en/function.glob.php)

Med disse tipsene og eksemplene bør du nå kunne enkelt og effektivt sjekke om en mappe eksisterer ved hjelp av PHP. Lykke til med kodingen!