---
title:    "Elm: Skrive tester"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester for koden din kan virke som en ekstra arbeidsoppgave, men det kan spare deg for mye tid og stress i det lange løp. Det kan hjelpe deg med å finne feil og feilsøke problemer før de blir et større problem. I tillegg kan det også bidra til å forbedre kvaliteten og påliteligheten til koden din.

## Slik gjør du det

Den mest populære måten å skrive tester i Elm på er ved å bruke biblioteket "elm-test". Først må du installere det ved å kjøre kommandoen `elm install elm-explorations/test` i terminalen din. Deretter kan du importere biblioteket og begynne å skrive tester.

```elm
import Test exposing (..)
import Expect

-- En enkel funksjon som doblet et tall
double : Int -> Int
double x = x * 2

-- En test for å sjekke om funksjonen dobler riktig
testDouble =
    describe "double function"
        [ test "doubles correctly" <|
            \() ->
                Expect.equal (double 5) 10
        ]
```

For å sjekke om testen fungerer, bruker du kommandoen `elm-test` i terminalen din. Du bør se en melding som sier at testen har passert. Dette betyr at funksjonen din fungerer som den skal.

## Dykk dypere

Å skrive tester i Elm handler ikke bare om å sjekke at koden din fungerer som den skal, men også om å lage pålitelige og robuste tester. Det er viktig å inkludere hjelpetester som sjekker hjørnetilfeller og uventede verdier for å sikre at koden din håndterer dem riktig.

En annen nyttig funksjon i "elm-test" er `testRunner`. Denne lar deg kjøre flere tester samtidig og generere en rapport. Du kan også lese mer om å skrive tester i Elm og utforske andre nyttige biblioteker som "elm-verify-examples" for å sikre at eksemplene i dokumentasjonen din fungerer som de skal.

## Se også

- [Elm Guide om testing](https://guide.elm-lang.org/testing/)
- [Elm-test dokumentasjon](https://package.elm-lang.org/packages/elm-explorations/test/latest)
- [elm-verify-examples dokumentasjon](https://package.elm-lang.org/packages/stoeffel/elm-verify-examples/latest/)