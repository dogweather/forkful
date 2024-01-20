---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Lengde på en streng i Fish Shell

## Hva & Hvorfor?

Å finne lengden på en streng handler om å telle antall tegn i en gitt tekststreng. Dette er nyttig for programmerere for å håndtere strengen på en mer kontrollert og effektiv måte, som å trimme den, dele den, osv.

## Hvordan:

Her er koden og eksempelet:

```Fish Shell 
string length -q "Hei Verden"
```

Det gir ut:

```Fish Shell 
11
```

En lengde av 11 betyr at det er 11 tegn i strengen "Hei Verden".

## Deep Dive:

Historisk sett har lengden på en streng vært en grunnleggende funksjon i mange programmeringsspråk. I Fish shell, har `string length`-kommandoen vært i prioritert fokus for å støtte behandling av strenger på en enkel og intuitiv måte.

Alternativt, hvis du ønsker å telle antall tegn manuelt i en streng, kan du bruke en `for`-løkke og en tellervariabel, men dette er mer tidkrevende enn å bruke innebygde kommandoer.

Implementasjon av `string length` i Fish shell handler om å telle antall tegn, inkludert mellomrom og spesialtegn. Dette er annerledes enn noen andre språk hvor blanke tegn ignoreres.

## Se også:

For mer informasjon og hjelp på Fish Shell programmering, besøk følgende ressurser. 

1. [Offisielle Fish Shell dokumentasjonen](https://fishshell.com/docs/current/index.html)
2. [Fish Shell Wiki](https://github.com/fish-shell/fish-shell/wiki)
3. [StackOverflow Fish Shell spørsmål](https://stackoverflow.com/questions/tagged/fish)