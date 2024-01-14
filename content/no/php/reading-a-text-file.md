---
title:                "PHP: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig oppgave for en PHP-programmerer. Det kan være nyttig når du jobber med store mengder data eller vil behandle informasjon som er lagret i en enkel tekstfil. Ettersom tekstfiler er enkle å opprette og lese, kan de være et viktig verktøy for å lagre og behandle informasjon i en PHP-applikasjon.

## Slik Gjør Du

For å lese en tekstfil i PHP, kan du bruke den innebygde funksjonen `fopen()` for å åpne filen, og deretter bruke `fgets()` for å lese linje for linje. Du kan også bruke en `while`-løkke for å lese filen til den er tom. Her er et eksempel på hvordan dette kan gjøres:

```PHP
$file = fopen("tekstfil.txt", "r"); // Åpne tekstfilen i lesemodus
while(!feof($file)){ // Sjekk om filen er tom
    $line = fgets($file); // Les en linje fra filen
    echo $line . "<br>"; // Skriv ut linjen
}
fclose($file); // Lukk filen
```

Output-en vil se slik ut:

```
Dette er en tekstfil.
Den inneholder noen linjer med informasjon.
Vi kan lese disse linjene en for en ved hjelp av PHP.
```

## Dykk Dypere

Å lese en tekstfil kan virke som en enkel oppgave, men det er noen ting å huske på for å unngå feil. Hvis filen er stor, kan det være lurt å bruke `file()`-funksjonen som leser hele filen og lagrer den i en array, istedenfor å lese den linje for linje. Dette kan føre til at programmet kjører raskere og mer effektivt. I tillegg er det viktig å huske på å lukke filen etter at du er ferdig med å lese den. Dette kan gjøres ved hjelp av `fclose()`-funksjonen.

## Se Også

- [PHP Manual for fopen()](https://www.php.net/manual/en/function.fopen.php)
- [PHP Manual for fgets()](https://www.php.net/manual/en/function.fgets.php)
- [PHP Manual for file()](https://www.php.net/manual/en/function.file.php)