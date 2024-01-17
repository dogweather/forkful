---
title:                "Slette tegn som matcher et mønster"
html_title:           "Fish Shell: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Sletting av tegn som matcher et mønster i koden din er en vanlig oppgave for programmerere. Dette betyr rett og slett å fjerne visse tegn fra en tekststreng basert på et bestemt mønster. Dette kan være nyttig for å rydde opp i koden din eller for å endre visse deler av tekststrengen.

# Hvordan:
```Fish Shell``` har et praktisk innebygd verktøy for å slette tegn som matcher et mønster. Dette verktøyet heter ```string replace``` og tar to argumenter: mønsteret som skal matches og teksten som det skal søkes i. La oss se på et eksempel:

```
string replace 'hello' 'Hi' 'Hello there!'
```

Dette vil resultere i ```Hi there!``` som output, da verktøyet har funnet og erstattet alle forekomster av ```hello``` med ```Hi```. Du kan også bruke regulære uttrykk som mønster for mer avanserte søk.

# Dypdykk:
Sletting av tegn som matcher et mønster har lenge vært en utfordring for programmerere. Tidligere måtte man bruke kompliserte regex-uttrykk eller eksterne verktøy for å oppnå dette, men med ```Fish Shell``` sitt ```string replace``` er det enklere enn noensinne.

Alternativene til ```string replace``` inkluderer andre kommandoer i ```Fish Shell``` som ```string split``` og ```string trim```. Disse kommandoene kan også brukes til å manipulere tekststrenger basert på visse mønstre.

# Se også:
- Fish Shell documentation: https://fishshell.com/docs/current/commands.html#string-replace
- Regular expressions tutorial: https://www.regular-expressions.info/tutorial.html