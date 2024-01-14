---
title:                "PHP: Sjekke om en mappe eksisterer"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer før du prøver å utføre operasjoner på den i PHP. Dette kan bidra til å unngå feil og sikre en jevn kjøring av koden din.

## Hvordan

For å sjekke om en mappe eksisterer i PHP, kan du bruke funksjonen `is_dir()`. Denne funksjonen tar inn som argument en streng som representerer mappen du ønsker å sjekke. Dersom mappen eksisterer, vil funksjonen returnere `true`, mens dersom den ikke eksisterer vil den returnere `false`.

```
PHP
$mappe_navn = "/stien/til/mappen";

if (is_dir($mappe_navn)) {
    echo "Mappen eksisterer!";
} else {
    echo "Mappen eksisterer ikke!";
}
```

Dersom du ønsker å sjekke om en mappe eksisterer relativt til din nåværende arbeidsmappe, kan du bruke funksjonen `chdir()` for å sette arbeidsmappen, og deretter bruke `is_dir()` til å sjekke om mappen eksisterer på samme måte som vist i eksempelet over.

```
PHP
// Sett arbeidsmappen til rotmappen
chdir("/");

// Sjekk om mappe eksisterer
if (is_dir("stien/til/mappen")) {
    echo "Mappen eksisterer!";
} else {
    echo "Mappen eksisterer ikke!";
}
```

## Dypdykk

Når du bruker funksjonen `is_dir()` for å sjekke om en mappe eksisterer, kan det være nyttig å vite at den også tar inn en valgfri parameter som lar deg sjekke om en *fil* med samme navn som mappen eksisterer. Dette kan være nyttig for å unngå konflikter med filer og mapper som deler samme navn.

Det er også viktig å merke seg at selv om funksjonen returnerer `false` dersom mappen ikke eksisterer, betyr det ikke nødvendigvis at det er en feil. Det kan hende at mappen ikke eksisterer, eller at stien du har angitt er feil. Det er derfor viktig å dobbeltsjekke stien og eventuelt bruke feilhåndtering for å sikre en problemfri kjøring av koden din.

## Se også

- Les mer om `is_dir()` funksjonen i PHPs offisielle dokumentasjon: [https://www.php.net/manual/en/function.is-dir.php](https://www.php.net/manual/en/function.is-dir.php)
- Lær mer om PHP inkludering av filer og sikkerhetstiltak for å unngå problemer med mappenavn: [https://www.php.net/manual/en/language.include.php](https://www.php.net/manual/en/language.include.php)
- Utforsk andre nyttige PHP funksjoner for å håndtere filer og mapper: [https://www.php.net/manual/en/ref.filesystem.php](https://www.php.net/manual/en/ref.filesystem.php)