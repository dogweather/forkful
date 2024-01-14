---
title:                "PHP: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en viktig del av PHP-programmering. Disse filene kan brukes til å midlertidig lagre data og bidra til å forbedre ytelsen til et nettsted. Det kan også være nyttig for å prosessere store datamengder eller for å teste forskjellige funksjoner uten å skrive til den faktiske databasen.

## Hvordan lage midlertidige filer i PHP

Midlertidige filer kan opprettes ved hjelp av PHPs innebygde funksjoner. En enkel måte å opprette en midlertidig fil på er å bruke "tempnam" -funksjonen, som automatisk genererer et unikt navn for filen og lagrer den på serveren.

```PHP
<?php
$tempfile = tempnam("/tmp", "TST");
echo "Det midlertidige filnavnet er " . $tempfile;
```

**Output:**
> Det midlertidige filnavnet er /tmp/TST1A2B3

Dette vil lage en midlertidig fil med navnet "TST1A2B3" i mappen "/tmp". Du kan også velge å spesifisere en bestemt plassering for den midlertidige filen.

En annen måte er å bruke "tmpfile" -funksjonen som automatisk oppretter og åpner en midlertidig fil og returnerer en pointer til den. Dette kan være nyttig hvis du trenger å gjøre flere operasjoner på samme midlertidige fil.

```PHP
<?php
$tempfile = tmpfile();
fwrite($tempfile, "Dette er en midlertidig fil som inneholder tekst.");
rewind($tempfile);
echo fread($tempfile, 40);
fclose($tempfile);
```

**Output:**
> Dette er en midlertidig fil som inneholder tekst.

Når du er ferdig med å bruke den midlertidige filen, må du huske å lukke den ved hjelp av "fclose" -funksjonen.

## Dypdykk

Det er viktig å merke seg at midlertidige filer ikke blir slettet automatisk. Du må sørge for å fjerne dem når de ikke lenger er nødvendige ved hjelp av "unlink" -funksjonen.

```PHP
<?php
$tempfile = tempnam("/tmp", "TST");
echo "Det midlertidige filnavnet er " . $tempfile;
// gjør operasjoner på den midlertidige filen
unlink($tempfile);
```

En annen ting å ta hensyn til er at midlertidige filer kan bli sårbare for sikkerhetsbrudd hvis de ikke håndteres riktig. Vær sikker på å sjekke og begrense tilgangen til den midlertidige filen for å unngå uautorisert tilgang.

## Se også

- PHP - midlertidige filer: https://www.php.net/manual/en/book.filesystem.php