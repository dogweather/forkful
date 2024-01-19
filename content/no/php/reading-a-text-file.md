---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er å trekke ut informasjon fra en lagret fil. Dette gjør at programmerere kan hente og manipulere data etter behov.

## Hvordan:

De to grunnleggende måtene å lese en tekstfil på i PHP er ved bruk av `file_get_contents()` funksjonen eller `fopen()` og `fgets()` funksjonene. Her er eksempler på begge:

```PHP 
// Bruke file_get_contents
$innhold = file_get_contents('eksempel.txt');
echo $innhold;

// Bruke fopen og fgets
$fil = fopen('eksempel.txt', 'r');
while (($linje = fgets($fil)) !== false) {
    echo $linje;
}
fclose($fil);
```

I begge tilfeller vil utgangen være innholdet i 'eksempel.txt' filen.

## Dypdykk: 

PHP ble opprettet i 1994 og har siden blitt et av de mest populære scriptspråkene for webutvikling. Fungerer som `file_get_contents`, `fopen` og `fgets` har eksistert siden tidlige PHP versjoner, deres betydning og bruk har bestått tidens test.

Alternativt til de overnevnte funksjonene, kan den mer moderne `file()` funksjonen brukes. `file()` leser en fil i en matrise, noe som kan være nyttig ved manipulering av store tekstfiler. 

I forhold til implementeringsdetaljer, husk at filatioperasjoner i PHP krever nøye håndtering av filrettigheter og feil. For eksempel, hvis `fopen()` ikke klarer å åpne filen, vil det returnere `FALSE` og utløse en advarsel.

## Se også:

Følgende kilder gir flere detaljer og alternativer for filoperasjoner i PHP:

1. Offisiell PHP-fildokumentasjon: https://www.php.net/manual/en/ref.filesystem.php
2. PHP Tutorial på W3School: https://www.w3schools.com/php/php_file.asp
3. Opplæring i PHP-filhåndtering på Geeks for Geeks: https://www.geeksforgeeks.org/php-file-handling/