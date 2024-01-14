---
title:    "PHP: Lese en tekstfil"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en viktig ferdighet for alle PHP-programmerere. Enten du skal behandle store mengder data eller bare trenger å hente informasjon fra en enkelt fil, er det nødvendig å kunne lese en tekstfil på en effektiv måte. Derfor er det viktig å forstå den grunnleggende syntaksen for å gjøre dette.

## Hvordan lese en tekstfil i PHP

Det første steget er å åpne filen ved å bruke fopen() funksjonen, som tar to parameter: filnavn og tilgangsmodus. For å lese filen i tekstmodus, bruker vi tilgangsmoduset "r". Deretter bruker vi en løkke til å lese linje for linje ved hjelp av fgets() funksjonen. Vi kan deretter behandle hver linje av teksten. Når vi er ferdig med å lese filen, må vi huske å lukke den ved hjelp av fclose() funksjonen.

``` PHP
$fil = fopen("tekstfil.txt", "r");

//Leser linje for linje og skriver ut til konsollen
while(!feof($fil)) {
    $linje = fgets($fil);
    echo $linje;
}

fclose($fil);
```

I eksempelet over brukte vi fgets() funksjonen for å lese en linje av teksten. Man kan også bruke fgetc() funksjonen for å lese et enkelt tegn av gangen.

## Dypdykk

I tillegg til de grunnleggende funksjonene nevnt ovenfor, finnes det også mange andre nyttige funksjoner som kan brukes til å lese tekstfiler, som for eksempel file_get_contents(), file() og readfile(). Disse funksjonene tilbyr forskjellige måter å lese og behandle filer på, og det kan være lurt å eksperimentere med dem for å finne ut hvilken som fungerer best for ditt spesifikke formål.

Det er også viktig å huske på at teksten som leses fra en fil kan være i en bestemt tegnkoding, og det kan være nødvendig å konvertere den til ønsket tegnkoding før man behandler den videre. Her kan funksjoner som mb_convert_encoding() være nyttige.

## Se også

For å lære mer om å jobbe med filer i PHP, kan du se på disse nyttige ressursene:

- PHP Dokumentasjon: http://php.net/manual/en/book.filesystem.php
- W3Schools: https://www.w3schools.com/php/php_file.asp