---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Regulære uttrykk lar deg søke og manipulere tekst basert på mønstre. Programmerere bruker det for effektivitet og nøyaktighet ved tekstbehandling.

## How to (Hvordan gjøre det)
Søk etter ord som starter med 'bok' i en tekstfil:

```Bash
grep '^bok' filnavn.txt
```

Finn og erstatt alle instanser av 'eple' med 'pære' i en fil:

```Bash
sed 's/eple/pære/g' filnavn.txt
```

Valider e-postadresser i en liste:

```Bash
grep -E "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$" epostliste.txt
```

## Deep Dive (Dypdykk)
Regulære uttrykk stammer fra teoretisk matematikk og ble populære i dataprogrammering med Unix. Alternativer inkluderer tekstbehandlingsverktøy slik som `awk` og `tr`. Implementasjonsdetaljer kan variere mellom verktøy og programmeringsspråk, men grunnleggende syntax er ganske konsekvent.

## See Also (Se Også)
- GNU Grep Documentation: https://www.gnu.org/software/grep/manual/grep.html
- Sed by Example: https://www.gnu.org/software/sed/manual/sed.html
- Regular-Expressions.info for et dypere dykk: https://www.regular-expressions.info/
