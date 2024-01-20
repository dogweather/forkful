---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Standard error" (stderr) er en output-kanal hvor PHP kan sende feil- eller statusmeldinger. Programmerere bruker den til å skille vanlig output fra feilmeldinger, noe som gjør feilsøking enklere.

## Hvordan:
For å skrive til stderr i PHP, bruk `fwrite()` til å sende en melding til STDERR-strømmen:

```PHP
<?php
fwrite(STDERR, "Dette er en feilmelding.\n");
?>
```

Eksempelutdata når du kjører i terminal:

```
Dette er en feilmelding.
```

En annen måte er å bruke `file_put_contents()` med `php://stderr`:

```PHP
<?php
file_put_contents('php://stderr', "Dette er en annen feilmelding.\n");
?>
```

## Dypdykk:
Historisk sett er stderr et konsept hentet fra Unix og C programmering, der det samme prinsippet brukes til å differensiere mellom standard output og error streams. I PHP kan du enten skrive direkte til stderr-streamen eller bruke funksjoner som `error_log()` for å logge feilmeldinger. `error_log()` kan konfigureres til å sende feilmeldinger til forskjellige destinasjoner, som logger, e-post eller andre systemer. Dette gir fleksibilitet i håndtering av feil under forskjellige runtime-miljøer.

## Se Også:
- PHP Manual on Standard Predefined Constants: https://www.php.net/manual/en/reserved.constants.php
- PHP Manual on `error_log()`: https://www.php.net/manual/en/function.error-log.php
- PHP Manual on `fwrite()`: https://www.php.net/manual/en/function.fwrite.php
- PHP Manual on Output Control Functions: https://www.php.net/manual/en/ref.outcontrol.php