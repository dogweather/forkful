---
title:                "Å skrive en tekstfil"
html_title:           "PHP: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil ved hjelp av PHP kan være nyttig i mange forskjellige situasjoner. For eksempel, hvis du ønsker å lagre informasjon fra et skjema på nettsiden din til en fil, eller hvis du vil generere en fil med dynamisk innhold basert på brukerens handlinger.

## Hvordan

For å skrive en tekstfil i PHP, må du bruke funksjonen "fopen" for å åpne en fil for skriving. Deretter kan du bruke funksjonen "fwrite" til å skrive ut data til filen. La oss se på et eksempel:

```PHP
$fil = fopen("ny_fil.txt", "w"); // Åpner filen "ny_fil.txt" for skriving
fwrite($fil, "Dette er en tekstfil skrevet i PHP."); // Skriver tekst til filen
fclose($fil); // Lukker filen
```

Etter å ha kjørt dette kodesnutten, vil du se at en ny tekstfil ved navn "ny_fil.txt" har blitt opprettet i samme mappe som PHP-filen din. Hvis du åpner filen, vil du se at teksten vi skrev til filen blir vist.

## Deep Dive

Nå som du vet grunnleggende om hvordan du skriver en tekstfil i PHP, kan det være nyttig å lære noen andre nyttige funksjoner som kan hjelpe deg med å håndtere filer. For eksempel kan du bruke "file_put_contents" funksjonen for å enkelt skrive tekst til en fil uten å åpne og lukke den manuelt.

```PHP
file_put_contents("ny_fil.txt", "Dette er en annen tekstfil skrevet i PHP.");
```

Du kan også bruke "fgets" eller "fgetcsv" funksjonene for å lese en tekstfil linje for linje eller en CSV-fil rad for rad. Disse funksjonene kan være nyttige hvis du trenger å behandle store tekstfiler eller datafiler.

## Se også

- [PHP-funksjoner for filbehandling](https://www.php.net/manual/en/ref.filesystem.php)
- [PHP filbehandling tutorial](https://www.php.net/manual/en/book.filesystem.php)