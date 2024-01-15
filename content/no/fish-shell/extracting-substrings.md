---
title:                "Utvinning av understrenger"
html_title:           "Fish Shell: Utvinning av understrenger"
simple_title:         "Utvinning av understrenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Hvorfor skal du bry deg om å trekke ut delstrenger? Det kan være svært nyttig når du prøver å manipulere og behandle tekstdata på en rask og effektiv måte.

## How To
Kodingseksempler og utførsel av utdata innenfor "```Fish Shell ... ```" kodelag kan være forvirrende å ta inn for nybegynnere. Men ikke bekymre deg, det er enklere enn det ser ut til.

For å trekke ut en delstreng i Fish Shell, bruker du kommandoen `string sub`, etterfulgt av startindeks og lengde på substringsen du ønsker å ekstrahere. Se nedenfor for et eksempel på hvordan dette ser ut i praksis:

```
Fish Shell $ string sub 3 5 "eksempeltekst"
[input/output] se
```

I eksempelet over trekker vi ut delstrengen "se" fra "eksempeltekst", med startindeks 3 og lengde 5. Du kan også bruke negative tall for å telle bakfra i strengen, for eksempel `string sub -4 3 "eksempeltekst"` for å få ut delstrengen "eks".

## Deep Dive
La oss dykke litt dypere inn i `string sub` kommandoen. I tillegg til å bruke startindeks og lengde, kan du også bruke `string sub` for å trekke ut delstrenger basert på et bestemt mønster eller uttrykk. For eksempel kan du bruke `string sub match "e(.*)t" "eksempeltekst"` for å få ut delstrengen "eks" basert på et regex-uttrykk.

En annen nyttig funksjon tilgjengelig med `string sub` er muligheten til å bruke variabler. Dette gjør det mulig å trekke ut delstrenger basert på variabelverdier i stedet for å hardkode dem. For eksempel kan du bruke `string sub $start $length "eksempeltekst"` for å få ut delstrengen basert på variablene `$start` og `$length`.

## See Also
Sjekk ut disse lenkene for mer informasjon om å trekke ut delstrenger i Fish Shell:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/string-sub.html)
- [RegExr - online regex tester](https://regexr.com/)