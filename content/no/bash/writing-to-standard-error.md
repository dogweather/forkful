---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skrive til standard error (stderr) er å sende feilmeldinger og diagnostikk separat fra vanlig utdata (stdout). Programmerere gjør dette for å logge feil uten å forstyrre programmets faktiske output.

## How to:
For å skrive til stderr i Bash, bruk `>&2`. Her er et eksempel:

```Bash
echo "Dette er en normal melding"
echo "Dette er en feilmelding" >&2
```

Kjør koden, og du vil se begge meldingene, men feilmeldingen er sendt til stderr.

## Deep Dive
I de tidlige dagene av Unix ble stderr introdusert for å skille feil fra ordinær output. Du kan omdirigere stderr til en fil eller et annet program. Alternativt kan `2>` brukes for å omdirigere feil. Selv om dagens programmeringsspråk har avanserte loggesystemer, forblir direkte skriving til stderr gjennom shell-scripting grunnleggende og nyttig i mange sammenhenger.

## See Also
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Stack Overflow, for praktiske spørsmål og svar: https://stackoverflow.com/questions/tagged/bash
