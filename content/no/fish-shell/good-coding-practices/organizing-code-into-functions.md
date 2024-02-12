---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-28T23:01:33.081296-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å samle biter av skript for å utføre spesifikke oppgaver. Vi gjør det fordi det gjør koden lettere å lese, teste og gjenbruke – ingen ønsker å vasse gjennom et sumpområde av kodespagetti.

## Hvordan:
I Fish skriver du en funksjon med `function`-nøkkelordet, gir den et navn, og avslutter med `end`. Her er en enkel en:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Utdata:
```
Hello, World!
```

Nå, la oss gjøre den så den hilser på en bruker:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Utdata:
```
Hey there, ditt_brukernavn!
```

For å lagre den på tvers av økter, bruk `funcsave greet`.

## Dykk dypere
Fish Shell-funksjoner er som mini-skript — du kan stappe ganske mye inn i dem. Historisk sett har konseptet med funksjoner i shell-skripting spart utallige timer med gjentakende skriving og feilsøking. I motsetning til programmeringsspråk som Python, handler Shell-funksjoner mer om bekvemmelighet enn struktur.

Noen shells, som Bash, bruker `function` eller rett og slett klammeparenteser. Fish holder seg til `function ... end`— klart og lesbart. Inne i Fish-funksjoner får du alle finessene: parametere, lokale variabler med `set -l`, og du kan til og med definere en funksjon inne i en annen funksjon.

Du trenger ikke en `return`-verdi fordi Fish ikke legger stor vekt på det; utdata fra din funksjon er dens retur. Og hvis du vil ha vedvarende funksjoner tilgjengelige for fremtidige økter, husk `funcsave`.

## Se også

- Fish-opplæringen om funksjoner: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Funksjonskommandoer

- [function](https://fishshell.com/docs/current/cmds/function.html) — Opprette en funksjon
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Skriv ut eller slett funksjoner
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Lagre definisjonen av en funksjon til brukerens autoload-katalog
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Interaktivt redigere en funksjon
