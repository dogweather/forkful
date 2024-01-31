---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av strenger innebærer å gjøre første bokstav i hvert ord til en stor forbokstav. Programmerere bruker det for å møte språknormer eller forbedre tekstutseende for brukergrensesnitt.

## Hvordan gjøre det:
I Fish Shell, forvandler du små til store forbokstaver slik:

```fish
function capitalize
    for word in $argv
        echo -n (string collect (string sub -s 1 -- $word | string upper) (string sub -s 2 -- $word)) " "
    end
    echo
end

# Bruk
capitalize "fisk er vennlig og fin"
```

Eksempel på utdata:

```
Fisk Er Vennlig Og Fin
```

## Dypdykk
I gamle dager brukte enkelte skall begrensede, innebygde strengoperasjoner. Med Fish, som er et moderne skall, får programmerere innebyggede strengfunksjoner som `string` som kan dele opp, erstatte og transformere tekst på kraftfulle måter. Alternativer til Fish-funksjonen `capitalize` kan inkludere bruk av `awk` eller kall til eksterne programmer som `sed`, men Fish tilbyr en renere og mer effektiv syntaks uten behov for eksterne verktøy. `string sub` og `string upper` er Fish-funksjoner som henholdsvis henter en delstreng og konverterer til store bokstaver.

## Se Også
- Offisiell Fish Shell-dokumentasjon: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- String-manipluasjon i Fish Shell: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Generell tekstbehandling i Unix-skall: [https://en.wikipedia.org/wiki/Text_processing](https://en.wikipedia.org/wiki/Text_processing)
