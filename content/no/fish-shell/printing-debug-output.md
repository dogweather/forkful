---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Fiskeskallet: Hvordan Skrive Ut Debug Informasjon

## Hva & Hvorfor?

Debug-utskrift er utskrivning av variabler eller tilstandsdata til konsollen for feilsøking. Dette gjør det lettere for programmerere å oppdage og rette feil i koden.

## Hvordan:

Hvis du vil skrive ut debugmeldinger i Fish Shell, kan du bruke `echo` kommandoen.

```Fish Shell
set variabel "Dette er en test"
echo "Debug: $variabel"
```

Når du kjører dette skriptet, vil det skrive ut følgende på konsollen:

```Fish Shell
Debug: Dette er en test
```

## Dyp Dykk

Debugging har vært grunnleggende for programmering siden oppstarten. Det gjør det mulig for utviklere å raskt identifisere og rette feil i koden.

I Fish Shell, som i mange andre skall, brukes `echo` kommandoen for å uttrykke variabler eller tilstandsdata. Mer avanserte alternativer kan være `printf` for mer komplekse formateringer, og `stderr` for utskrift av feil til konsollen.

For implementert debugprint, husk å fjern eller kommenter ut disse linjene når du er ferdig. Hvis du ikke gjør det, vil endelige brukere se disse debugmeldingene, noe som kan forvirre dem.

## Se Også:

1. [Fiskeskalldokumentasjon for `echo`](https://fishshell.com/docs/current/cmds/echo.html)
2. [Fiskeskalldokumentasjon for `printf`](https://fishshell.com/docs/current/cmds/printf.html)
3. [Fiskeskalldokumentasjon for `stderr`](https://fishshell.com/docs/current/tutorial.html#tut_redirects)