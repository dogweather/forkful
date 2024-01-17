---
title:                "Søking og erstattin"
html_title:           "Haskell: Søking og erstattin"
simple_title:         "Søking og erstattin"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Søking og erstatting av tekst er en vanlig oppgave for programmerere. Det handler om å finne et bestemt ord eller uttrykk i en tekst og erstatte det med noe annet. Programmerere gjør dette for å effektivt endre og oppdatere store mengder data i en enkel operasjon.

# Hvordan:
Haskell har innebygd funksjonalitet for søking og erstatting av tekst. Du kan bruke funksjonene "strSub" og "strSubGlobal" for å erstatte et bestemt uttrykk i en tekst, enten bare en gang eller over hele teksten. Se et enkelt eksempel på bruken av disse funksjonene nedenfor.

```Haskell
strSub "Hello" "Goodbye" "Hello, World!" -- output: "Goodbye, World!"
strSubGlobal "un" "in" "unbelievable" -- output: "inbelievable"
```

# Dykk dypere:
Søking og erstatting av tekst har vært en del av programmering siden tidlig på 70-tallet, da ed-kommandoen "s/" ble introdusert på UNIX-operativsystemet. I dag er det mange verktøy og editorer som tilbyr avanserte funksjoner for søking og erstatting, som f.eks. Vim og Sublime Text. I tillegg til "strSub" og "strSubGlobal", har Haskell også andre nyttige funksjoner som "strRep" og "showSubs" for å manipulere og inspisere tekst. Implementasjonen av disse funksjonene er basert på effektive algoritmer som gjør dem raske og pålitelige.

# Se også:
- [Haskell Dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell Øvelser](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)
- [Gentoo Haskell Prosjekt](https://wiki.gentoo.org/wiki/Project:Haskell)