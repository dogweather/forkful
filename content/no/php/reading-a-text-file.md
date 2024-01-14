---
title:    "PHP: Å lese en tekstfil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor
Å lese en tekstfil kan være en nyttig ferdighet å ha for enhver som jobber med PHP. Det lar deg få tilgang til informasjonen som er lagret i filen, og gjør det mulig å behandle og manipulere den på forskjellige måter.

## Hvordan gjøre det
```PHP
<?php
// Åpne filen med navnet "test.txt" for lesing
$file = fopen("test.txt", "r");

// Sjekk om filen er åpnet succesfuldt
if (!$file) {
  die("Kan ikke åpne filen!");
}

// Les innholdet av filen, linje for linje
while(!feof($file)) {
  echo fgets($file) . '<br>';
}

// Lukk filen
fclose($file);
?>
```

**Output:**

Dette er en testfil

PHP er et flott programmeringsspråk

## Dykk dypere
Når du leser en tekstfil i PHP, kan du også bruke forskjellige metoder for å behandle og manipulere informasjonen som er lagret i filen. Du kan for eksempel bruke `file()`-funksjonen for å lese filen som en array av linjer, eller `fgetcsv()`-funksjonen for å lese en CSV-fil.

En annen nyttig funksjon er `file_get_contents()`, som lar deg lese hele innholdet av en fil som en streng. Dette kan være nyttig hvis du bare trenger å behandle en liten tekstfil raskt.

Det er også viktig å nevne at når du leser en tekstfil, må du være oppmerksom på formateringen av filen. Hvis det er spesiell formatering, for eksempel med komma-separerte verdier, må du sørge for at du behandler dette på riktig måte.

## Se også
- [PHP Filbehandling](https://www.php.net/manual/en/book.filesystem.php)
- [PHP File funksjoner](https://www.php.net/manual/en/ref.filesystem.php)
- [PHP File systemet](https://www.php.net/manual/en/book.filesystem.php) (fra PHP-manualen)