---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Strengkonkatenering handler om å binde sammen to eller flere tekststrenger til én. Programmerere gjør dette for å effektivt lage nye tekster fra eksisterende deler.

## Hvordan:

Her ser du hvordan du kan kombinere strenger i Fish Shell:

```Fish Shell
set streng1 "Hallo, "
set streng2 "Norge!"
echo $streng1$streng2
```

Utdata:

```Fish Shell
Hallo, Norge!
```
## Dypdykk

Strengkonkatenering har lange historiske røtter i programmering, fra de tidlige dager av Perl og Unix-shell-scripting. I Fish Shell, utføres denne oppgaven enkelt uten behov for spesielle operatorer.

Det er flere måter å oppnå det samme på i andre programmeringsspråk. For eksempel, i Python kan du bruke '+' operatoren (`streng1 + streng2`), og i Javascript kan du bruke både '+' og '`'. Begge tilnærmingene gir tilnærmet samme resultat, men forskjellen ligger i detaljene rundt ytelse og lesbarhet.

Fish Shell tilbyr ikke noen innebyggede funksjoner for strengkonkatenering fordi det er unødvendig. Ved å bruke variabel-navnet direkte, kan du kombinere tekstinnholdet uten noen ekstra kode. Dette forenkler lesbarheten av koden og forbedrer ytelsen.

## Se Også

For mer informasjon om hvordan å jobbe med tekststrenger i Fish, sjekk ut følgende ressurser:

- Fish sin offisielle dokumentasjon: https://fishshell.com/docs/current/index.html
- Fish scripting tutorial: https://fishshell.com/docs/current/tutorial.html
- Håndtering av tekststrenger i Fish: https://fishshell.com/docs/current/commands.html#string
- StackOverflows diskusjon om strengoperasjoner i Fish: https://stackoverflow.com/questions/21138413/fish-shell-string-operations