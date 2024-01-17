---
title:                "Å skrive tester"
html_title:           "Elm: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skriving av tester er en viktig del av å være en dyktig programmerer. Det er en prosess hvor man skriver kode for å sjekke om den eksisterende koden fungerer som den skal. Dette gjør man for å sikre seg at koden fungerer som den skal, og for å redusere sjansene for feil og bugs.

## Slik gjør du det:

Enkelt sagt, det å skrive tester innebærer å lage små kodeeksempler som verifiserer at funksjonene og metodene i programmet oppfører seg som forventet. La oss ta en titt på et eksempel:

```Elm
import List exposing (head)

testHead =
  describe "List.head"
    [ test "returns the first element of a non-empty list" <|
        \_ -> head [1,2,3] == Just 1
    ]
```

I dette eksempelet ser vi hvordan vi benytter Elm's `List.head` funksjon i en test. Ved hjelp av `test` funksjonen, kan vi sjekke om det returnerte verdien av `head` funksjonen er det vi forventer. Her benytter vi også `describe` funksjonen for å gi en beskrivelse av hva testen skal verifisere.

## Dypdykk:

Tester har blitt en nødvendighet i moderne programmering, og mange programmeringsspråk har lignende verktøy som lar utviklere skrive og kjøre tester. I Elm, benytter man seg av et bibliotek som heter `elm-test` for å skrive tester og kjøre dem.

Alternativt, kan man også benytte seg av andre biblioteker og rammeverk som tilbyr tester som en del av sin funksjonalitet. Noen eksempler inkluderer `elm-spec` og `elm-explorations/test`.

## Se også:

[Elm Test Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/Elm-Test)

[Elm Spec Documentation](https://package.elm-lang.org/packages/JustusWeber/elm-spec/latest/)

[Elm Explorations Test Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)

Så nå vet du hva det vil si å skrive tester i Elm, og hvorfor det er viktig. Start å implementer testing i din egen kode og se hvor mye det kan hjelpe deg som programmerer.