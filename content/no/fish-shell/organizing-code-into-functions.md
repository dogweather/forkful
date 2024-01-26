---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-26T01:10:24.649907-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Organisering av kode i funksjoner handler om å gruppere biter av script for å utføre spesifikke oppgaver. Vi gjør dette fordi det gjør koden lettere å lese, teste og gjenbruke — ingen ønsker å vasse gjennom en sump av kodespagetti.

## Hvordan gjøre det:
I Fish skriver du en funksjon med nøkkelordet `function`, gir den et navn, og avslutter med `end`. Her er en enkel en:

```fish
function hello
    echo "Hei, Verden!"
end

hello
```

Utdata:
```
Hei, Verden!
```

Nå, la oss gjøre det slik at den hilser på en bruker:

```fish
function greet
    set user (whoami)
    echo "Hei der, $user!"
end

greet
```

Utdata:
```
Hei der, ditt_brukernavn!
```

For å lagre den på tvers av økter, bruk `funcsave greet`.

## Dypdykk
Funksjoner i Fish Shell er som mini-script — du kan putte nesten hva som helst der inne. Historisk sett har konseptet med funksjoner i shell-scripting spart utallige timer med repetitivt tastearbeid og feilsøking. I motsetning til programmeringsspråk som Python, er Shell-funksjoner mer for bekvemmelighet enn struktur.

Noen skall, som Bash, bruker nøkkelordet `function` eller rett og slett krøllparenteser. Fish holder seg til `function ... end` — klart og lesbart. Inne i Fish-funksjoner får du alle finesser: parametere, lokale variabler med `set -l`, og du kan til og med definere en funksjon inne i en annen funksjon.

Du trenger ikke en `return`-verdi fordi Fish ikke legger stor vekt på dette; utdata fra funksjonen din er dens returverdi. Og hvis du vil ha vedvarende funksjoner tilgjengelige for fremtidige økter, husk `funcsave`.

## Se også
- Fish-tutorialen om funksjoner: https://fishshell.com/docs/current/tutorial.html#tut_functions
- Fish-dokumentasjonen for `function`: https://fishshell.com/docs/current/cmds/function.html
- En omfattende guide til å skrive funksjoner i fish: https://fishshell.com/docs/current/index.html#syntax-function