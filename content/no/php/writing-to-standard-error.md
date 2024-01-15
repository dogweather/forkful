---
title:                "Skriving til standardfeil"
html_title:           "PHP: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor?

Det er mange grunner til å skrive til standard error i PHP-kode. Noen ganger vil du kanskje vise en feilmelding til brukeren, eller logge feil for senere å finne og fikse problemer. Uansett hva årsaken er, kan skriving til standard error være et nyttig verktøy i utviklingsprosessen.

## Hvordan gjør du det?

Det er enkelt å skrive til standard error i PHP, du trenger bare å bruke funksjonen `fwrite()` og pekeren `STDERR`. Her er et eksempel:

```PHP
<?php
fwrite(STDERR, "Dette er en feilmelding som blir skrevet til standard error.");
```

Dette vil skrive teksten "Dette er en feilmelding som blir skrevet til standard error." til standard error, som vanligvis vil vises i terminalen eller loggfiler, avhengig av teknologien du bruker.

## Dypdykk

Når du skriver til standard error, er det viktig å huske på at dette er annerledes enn å skrive til standard output (som vanligvis er terminalen eller nettleseren). Standard error brukes spesielt for å vise feil og unntak, mens standard output er beregnet for vanlig utdata og meldinger til brukeren.

Det er også verdt å nevne at `fwrite()` funksjonen vil returnere antall skrevne bytes, som kan være nyttig for å feilsøke mulige problemer.

## Se også

- [PHP fwrite() Function](https://www.w3schools.com/php/func_filesystem_fwrite.asp)
- [PHP stderr](https://www.php.net/manual/en/wrappers.php.php)
- [Logging in PHP](https://www.digitalocean.com/community/tutorials/how-to-use-logging-in-php)