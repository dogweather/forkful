---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Tilfeldige numre er dataverdier generert av en prosess som er uforutsigbar, noe som gir mangfoldet i dataene. Dette hjelper programmerere til å opprette realistiske simuleringer, sikkerhetsprotokoller og mange flere applikasjoner.

## Hvordan gjøre dette:

Generering av tilfeldige tall i Fish Shell er ganske enkel. Du kan bruke 'random' -kommandoen til dette formålet. Følgende eksempler vil hjelpe deg å bedre forstå bruk av denne kommandoen:

```Fish Shell
# Generere et tilfeldig tall i Fish Shell
random
```

Utgang:

```Fish Shell
179424673
```

Du kan også spesifisere et område for tilfeldige tall.

```Fish Shell
# Generere et tall mellom 1 og 10
random 1 10
```

Utgang:

```Fish Shell
7
```

## Dypdykk:

"Random"-kommandoen i Fish Shell ble introdusert i versjon 2.3. Det bruker c++ standardbiblioteket til å generere pseudo-tilfeldige tall. Alternativene inkluderer bruk av eksterne programmer som 'shuf' eller 'jot', men 'random'-kommandoen gir en mye enklere og mer idiomatisk måte å generere tilfeldige tall på i Fish.

En interessant fakta angående generering av tilfeldige tall er at det ikke er noen ekte "tilfeldige" numre i beregninger, bare pseudo-tilfeldige. Det betyr at det er en algoritme med et startpunkt ("frø") som produserer sekvensen, og hvis du vet dette frøet, kan du reprodusere sekvensen.

## Se også:

For mer detaljert informasjon om "random"-kommandoen, se Fish Shell-dokumentasjonen: https://fishshell.com/docs/current/cmds/random.html.

Du kan også finne mer informasjon om pseudotilfeldige nummergeneratorer på Wikipedia: https://no.wikipedia.org/wiki/Pseudotilfeldig_nummergenerator.