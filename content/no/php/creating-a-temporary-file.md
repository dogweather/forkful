---
title:    "PHP: Opprettelse av en midlertidig fil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor
Å lage midlertidige filer i PHP kan være nyttig for å midlertidig lagre data eller utføre enkelte oppgaver. Det kan også være nyttig når du jobber med sikkerhet og vil sørge for at sensitive data ikke lagres permanent.

## Hvordan
Å lage en midlertidig fil i PHP er enkelt. Du kan bruke funksjonen "tempnam" for å generere en unik filbane og navn for den midlertidige filen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
<?php
// Opprett en midlertidig filbane og navn
$temp_file = tempnam(sys_get_temp_dir(), 'prefix_');

// Åpne filen for skriving
$handle = fopen($temp_file, 'w');

// Skriv data til filen
fwrite($handle, 'Her er noen data som skal lagres i den midlertidige filen');

// Lukk filen når du er ferdig med å skrive til den
fclose($handle);

// Vis plasseringen til den midlertidige filen
echo $temp_file;
```

Og her er et eksempel på den genererte outputen:

```shell
/var/folders/c6/bdb35lf97g1_1ycqcllr73x00000gn/T/prefix_SjLM
```

## Dypdykk
Når du oppretter en midlertidig fil, er det viktig å huske at denne filen vil slettes så snart skriptet er ferdig med å kjøre. Dette betyr at hvis du trenger å beholde dataene i filen, må du lagre dem et annet sted før filen blir slettet.

Det er også verdt å nevne at midlertidige filer kan bli gjenstand for angrep hvis de ikke håndteres riktig. Det er derfor viktig å sjekke filbanen og navnet generert av "tempnam" for å sikre at det ikke er mulig for noen å gjette og få tilgang til filen.

## Se også
- [Tempnam dokumentasjon](https://www.php.net/manual/en/function.tempnam.php)
- [Hvordan håndtere midlertidige filer i PHP](https://www.php.net/manual/en/features.file-upload.php#118534)
- [Sikkerhetsrisikoer ved bruk av midlertidige filer i PHP](https://security.stackexchange.com/questions/196138/are-temporary-files-in-php-a-security-risk)