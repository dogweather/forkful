---
title:                "Å lese en tekstdokument"
html_title:           "PHP: Å lese en tekstdokument"
simple_title:         "Å lese en tekstdokument"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en vanlig oppgave innen webutvikling, og en nyttig ferdighet å ha for enhver PHP-utvikler. Ved å lære hvordan man leser og behandler tekstfiler i PHP, kan man enkelt manipulere data og bygge dynamiske og responsive nettsteder.

## Hvordan gjøre det

Å lese en tekstfil i PHP er en enkel og rett frem oppgave. Først må man åpne filen ved hjelp av `fopen()` funksjonen og angir filbanen og tilgangsmetoden (lesing, skriving, etc.). Deretter kan man bruke `fgets()` funksjonen til å lese hver enkelt linje i filen og behandle den som ønsket. Til slutt må man huske å lukke filen med `fclose()` funksjonen.

```PHP
<?php
$fil = fopen("tekstfil.txt", "r"); // åpner filen i lesing-modus
while(!feof($fil)) { // leser hver linje til slutten av filen er nådd
    $linje = fgets($fil); // lagrer linjen i en variabel
    echo $linje; // printer ut linjen
}
fclose($fil); // lukker filen
?>
```

## Dypdykk

Ved å bruke `fgets()` funksjonen, kan man lese en tekstfil linje for linje. Det finnes imidlertid også andre funksjoner som kan brukes for å lese og behandle tekstfiler i PHP, for eksempel `file_get_contents()` og `file()`.

En annen nyttig funksjon er `fgetcsv()` som kan brukes til å lese en CSV-fil (Comma Separated Values), vanligvis brukt for å lagre tabelldata. Denne funksjonen leser hver linje i filen og deler den opp i separate verdier basert på et angitt separator-tegn, for eksempel `,` eller `;`.

```PHP
<?php
$fil = fopen("data.csv", "r");
while(!feof($fil)) {
    $data = fgetcsv($fil, 1000, ";"); // angir et semikolon som separasjonstegn
    print_r($data); // printer ut en array med de forskjellige verdiene
}
fclose($fil);
?>
```

## Se også

- [PHP Dokumentasjon - Filbehandling](https://www.php.net/manual/en/book.filesystem.php)
- [W3Schools - PHP Filbehandling](https://www.w3schools.com/php/php_file.asp)
- [PHPKode - Lesing av CSV-filer i PHP](https://www.phpkode.com/tutorials/php-file-handling-tutorial/reading-csv-file-in-php/)