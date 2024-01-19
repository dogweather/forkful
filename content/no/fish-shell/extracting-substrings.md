---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I programmering er tråduttrekk, å hente en del av en større streng basert på oppgitte indekser. Programmerere gjør dette for å manipulere og bruke spesifikke opplysninger i en datastruktur.

## Hvordan:

Her er noen eksempler på hvordan du trekker ut substrings i Fish Shell:

```Fish Shell
set streng 'Hei verden'
echo $streng[1 3] # output: 'Hei'
```

```Fish Shell
set navn 'Ola Nordmann'
echo $navn[1 3] # output: 'Ola'
```

```Fish Shell
set sitat 'Fisk er gøy'
echo $sitat[2 7] # output: 'er gøy'
```

## Dypdykk:

Historisk kontekst: Fish Shell, første gang utgitt i 2005, har alltid hatt evnen til å trekke ut substrings. Det skyldes at funksjonaliteten er et fundamentalt konsept i de fleste programmeringsspråk.

Alternativer: Andre skall, som Bash og Zsh, har også muligheter for å trekke ut substrings. Syntaksen kan variere litt mellom skallene.

Implementeringsdetaljer: I Fish Shell utføres substringuttrekk ved å bruke en 'set' kommando for å definere strengen, og deretter bruke indeksverdier innenfor firkantparentesene til å spesifisere den delen du vil trekke ut.

## Se Også:

For mer informasjon om substrings og Fish Shell, sjekk ut disse kildene:

1. Fish Shell dokumentasjon: [Fish Shell Docs](https://fishshell.com/docs/current/index.html)
2. En detaljert guide om string manipulering i Fish Shell: [String Manipulation in Fish](https://devhints.io/fish-shell)
3. En introduksjon til programmering i Fish Shell: [Intro to Fish](https://fishshell.com/docs/current/tutorial.html)