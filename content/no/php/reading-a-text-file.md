---
title:                "PHP: Lesing av en tekstfil"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lese en tekstfil kan være en viktig del av programmering, spesielt når det kommer til å hente ut data eller informasjon fra eksterne kilder. Ved å lære hvordan du kan lese en tekstfil, vil du kunne åpne opp for mange muligheter og muliggjøre mer kompleks og effektiv programmering.

## Hvordan
For å lese en tekstfil i PHP, kan du bruke funksjonen `file_get_contents()`. Denne funksjonen tar inn en filsti og returnerer innholdet i filen som en string. For å demonstrere dette, la oss si at vi har en tekstfil kalt "hello.txt" som inneholder teksten "Hei verden!". For å lese denne filen og skrive ut innholdet, kan koden se slik ut:

```PHP
<?php
$filinnhold = file_get_contents("hello.txt");
echo $filinnhold; // Skriver ut "Hei verden!"
```

## Deep Dive
Det finnes flere måter å lese en tekstfil på i PHP, blant annet ved hjelp av funksjonene `fopen()` og `fread()`. Disse funksjonene åpner en fil og leser innholdet i mindre biter, noe som kan være nyttig hvis du har å gjøre med store filer eller ønsker å manipulere dataene på en mer detaljert måte. Du kan også velge å lese bare en del av filen ved å spesifisere start- og slutt-posisjon i `fread()`.

Det er også viktig å være oppmerksom på sikkerhet når du leser en tekstfil. Pass på å validere og sikre filstien før du leser innholdet, for å unngå uautorisert tilgang til filer på serveren.

## Se også
- [PHP manual for file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP manual for fread()](https://www.php.net/manual/en/function.fread.php)
- [Nettstedet W3Schools for mer informasjon om lesing av tekstfiler i PHP](https://www.w3schools.com/php/php_file_open.asp)