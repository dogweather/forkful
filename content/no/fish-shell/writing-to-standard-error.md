---
title:                "Skriving til standard error"
html_title:           "Fish Shell: Skriving til standard error"
simple_title:         "Skriving til standard error"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Hvis du noen gang har programmert eller jobbet med terminalen, har du sannsynligvis sett kommandoer som bruker ">&2" eller "2>" til å skrive til standard error. Dette er måter for programmerere å sende feilmeldinger eller annen informasjon til terminalen.

Å skrive til standard error kan hjelpe med feilsøking og debugging i programmer. Det lar utviklere sende spesifikk informasjon til terminalen som kan hjelpe dem med å finne og løse feil.

## Hvordan:

For å skrive til standard error i Fish Shell, bruker du kommandoen "echo" med flagget "-e" og spesifiserer at du vil skrive til standard error ved å bruke ">&2". Se eksempelet nedenfor for å se hvordan dette ser ut i praksis.

```Fish Shell
echo -e "Dette er en feilmelding" >&2
```

Dette vil skrive ut teksten "Dette er en feilmelding" til standard error, som vanligvis vil bli vist i rødt i terminalen.

## Dypdykk:

Skriving til standard error har vært en del av programmering i mange år, og er en del av standarden POSIX som beskriver hvordan Unix-baserte systemer skal fungere. Noen programmerere bruker også kommandoen "error" i stedet for "echo" for å skrive til standard error.

Selv om å skrive til standard error kan være nyttig, kan det også føre til rot og kaos hvis det misbrukes. Det kan være lurt å begrense bruken av det til kun feil- og debuggingsmeldinger for å unngå å forvirre brukeren.

## Se også:

- Fish Shell dokumentasjon: https://fishshell.com/docs/current/
- Forskjellen mellom standard output og standard error: https://stackoverflow.com/questions/983779/difference-between-stderr-and-stdout
- Alt om POSIX-standardene: https://en.wikipedia.org/wiki/POSIX