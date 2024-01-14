---
title:                "Elm: Skriving av tester"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens komplekse programvareverden er det viktigere enn noensinne å sørge for at koden vår fungerer som den skal. Testing er en viktig del av denne prosessen, og kan hjelpe oss med å finne og fikse feil før de skaper problemer i produksjon. Ved å skrive tester, kan vi øke vår tillit til koden vår og bidra til å skape robuste og pålitelige programmer. 

## Slik gjør du det

For å skrive tester i Elm, kan vi bruke et dedikert bibliotek som heter `elm-test`. Dette lar oss lage automatiserte tester som verifiserer at koden vår oppfører seg som forventet. La oss ta en titt på et enkelt eksempel:

```Elm
import Expect exposing (equal)

-- En funksjon som legger sammen to tall
add : Int -> Int -> Int
add x y =
    x + y

tests =
    describe "add funksjonen"
        [ test "adderer to positive tall" <|
            \_ ->
                Expect.equal (add 2 3) 5
        , test "adderer negative og positive tall" <|
            \_ ->
                Expect.equal (add -5 8) 3
        , test "adderer null og et tall" <|
            \_ ->
                Expect.equal (add 0 11) 11
        ]

-- Kjører testene og viser resultat
main =
    Elm.Test.run tests
```

I dette eksemplet lager vi en test for en `add`-funksjon som vi antar å allerede ha definert et sted i koden vår. Vi bruker `describe` og `test` funksjonene fra `elm-test` for å organisere og kjøre testene våre. Deretter sammenligner vi resultatet av å kalle `add` med noen forventede verdier ved hjelp av `Expect.equal`. Når vi kjører dette eksemplet, vil vi få en melding om at alle testene har bestått.

## Dypdykk

Når vi skriver tester i Elm, er det viktig å forstå at de også er en del av selve koden vår. Dette betyr at vi kan bruke alle de gode prinsippene for god koding også på våre tester. For eksempel kan vi utnytte modulsystemet i Elm for å organisere og gjenbruke våre tester på en ryddig måte.

Vi kan også dra nytte av Elm sin statisk typede natur for å sikre at testene våre bare faktisk kan gå gjennom hvis koden vår oppfyller de forventede typene. Dette bidrar til å fange feil tidlig og reduserer risikoen for uventede resultater når koden vår endres.

For en mer omfattende oversikt over testing i Elm, kan du sjekke ut officielle dokumentasjonen og følge med på samfunnetes beste praksis.

## Se også

- [Elm Test-dokumentasjon](https://package.elm-lang.org/packages/elm-explorations/test/latest)
- [Beste praksis for testing i Elm](https://guide.elm-lang.org/appendix/testing.html) 
- [Samfunnets video om testing i Elm](https://www.youtube.com/watch?v=F8qUNz64LCo)