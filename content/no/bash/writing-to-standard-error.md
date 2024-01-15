---
title:                "Skriving til standardfeil"
html_title:           "Bash: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å skrive til standardfeil i Bash. Det kan for eksempel være for å få mer informasjon om feil som skjer under kjøring av et skript, eller for å vise spesifikk informasjon til brukeren.

## Hvordan

Bash har en egen kommando for å skrive til standardfeil – `>&2`. Dette sender utdata til standardfeil istedenfor standardutgang. La oss se på et eksempel:

```Bash
#!/bin/bash
echo "Dette er en melding til standardutgang"
>&2 echo "Dette er en melding til standardfeil"
```

Dette vil gi følgende utdata når skriptet kjøres:

```
Dette er en melding til standardutgang
Dette er en melding til standardfeil
```

Som du ser, blir meldingen med `>&2` kommandoen skrevet ut til standardfeil istedenfor standardutgang. Dette kan være nyttig hvis du ønsker å skille mellom forskjellige typer utdata eller gi mer detaljert informasjon til brukeren.

## Dypdykk

Når du bruker `>&2` kommandoen, blir utdataen sendt direkte til standardfeil uten noen form for formatering. Dette betyr at det ikke blir lagt til noen linjeskift eller formatering. Det kan derfor være lurt å inkludere dette i en echo-kommando for å få ønsket format på utdataen. La oss se på et eksempel:

```Bash
#!/bin/bash
ERROR_MSG="Oops, noe gikk galt!"
echo "Dette er en melding til standardutgang"
>&2 echo -e "Feil: $ERROR_MSG\n"
```

Her har vi lagt til `-e` flagget til echo-kommandoen for å aktivere formatering. Dette gjør at vi kan legge til linjeskift og annen formatering i meldingen til standardfeil. Ved å bruke `>&2` kommandoen sammen med echo-kommandoen, sørger vi for at disse formateringene blir sendt til standardfeil og ikke standardutgang.

For å lære mer om hvordan du kan behandle feil i Bash-skript, kan du lese denne artikkelen: [Behandling av feil i Bash-skript](https://www.unix.com/tutorials/466746-bash-error-handling.html).

## Se Også

- [Offisiell Bash-dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-skripting på W3Schools](https://www.w3schools.com/whatis/whatis_bash.asp)
- [Begynnerguide til Bash-skripting](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bruk av exit-kommando i Bash-skript](https://linuxhandbook.com/exit-command-in-bash/)