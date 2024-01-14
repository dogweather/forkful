---
title:                "Haskell: Å starte et nytt prosjekt"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Starte et nytt prosjekt kan virke som en overveldende oppgave, men det kan også være en spennende og givende opplevelse. Ved å lære Haskell og utvikle dine egne prosjekter, vil du få en dypere forståelse for funksjonell programmering og kunne utvide dine programmeringsferdigheter.

## Hvordan

For å starte et nytt Haskell-prosjekt, må du først sørge for at du har installert GHC (Glasgow Haskell Compiler) og Cabal (et bygg- og pakkesystem for Haskell). Deretter kan du opprette et nytt prosjekt ved å følge disse trinnene:

1. Åpne et terminalvindu og naviger til en mappe der du ønsker å lagre prosjektet ditt.
2. Skriv følgende kommando for å opprette en ny mappe for prosjektet ditt: ```mkdir project-name```
3. Gå inn i den nye mappen ved å skrive: ```cd project-name```
4. Skriv kommandoen ```cabal init``` for å opprette en ny konfigurasjonsfil for prosjektet ditt.
5. Deretter vil du bli bedt om å svare på noen spørsmål om prosjektet ditt, som navn, forfatter, beskrivelse osv. Du kan også la noen felter være tomme og gå videre til neste trinn.
6. Dette vil opprette en mappestruktur for prosjektet ditt og en cabal fil som holder konfigurasjonsinformasjonen din.

Etter at du har satt opp prosjektet ditt, kan du begynne å kode i Haskell ved å åpne filen som heter ```Main.hs``` i ```app```-mappen. Du kan også legge til avhengigheter til prosjektet ditt ved å redigere cabal-filen og deretter kjøre ```cabal install``` i terminalen for å installere dem.

## Dykk dypere

Når du har satt opp prosjektet ditt, kan du begynne å utforske ulike biblioteker og funksjoner i Haskell for å bygge et mer komplekst program. Du kan også bruke konseptene du har lært i kurs eller på nettet til å skrive mer effektiv kode og implementere nye funksjoner.

Det er også viktig å huske på å organisere koden din på en strukturert og lesbar måte. Dette vil ikke bare gjøre det enklere for deg å implementere nye funksjoner, men det vil også gjøre koden din mer vedlikeholdsvennlig og forståelig for andre.

## Se også

- [Offisiell Haskell-hjemmeside](https://www.haskell.org/)
- [Haskell Tutorial for Beginners](https://www.tutorialspoint.com/haskell/index.htm)
- [Haskell Programming From First Principles](https://haskellbook.com/)