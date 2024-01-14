---
title:                "Haskell: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor skrive tester i Haskell

Å skrive tester er en viktig del av enhver programmeringsprosess, og det gjelder også for Haskell. Ved å skrive tester kan du sikre at koden din fungerer som den skal, og redusere feil og bugs. I tillegg kan det bidra til å forbedre den generelle kvaliteten på koden din.

## Hvordan skrive tester i Haskell

For å skrive tester i Haskell, må du først importere testing-rammeverket "Hspec" ved å bruke følgende kode:

```Haskell 
import Test.Hspec 
```

Deretter kan du definere tester ved å bruke funksjonen `describe`, som tar inn en beskrivelse av hva testen skal sjekke og en funksjon som inneholder selve testen. Se et eksempel på kode nedenfor:

```Haskell
describe "Enkle matematikkfunksjoner" $ do
  it "Skal beregne summen av to tall" $ do
    sum 2 3 `shouldBe` 5
```

I dette eksempelet vanligvis forvente vi at summen av 2 og 3 er lik 5. `shouldBe` funksjonen sjekker om dette er tilfelle, og vil returnere en feil hvis forventningen ikke stemmer. Det er også mulig å bruke andre tester som `shouldSatisfy`, `shouldThrow` og `shouldReturn` avhengig av hva som skal testes. Det er viktig å også inkludere noen eksempler på hva som forventes å fungere for at testene skal være effektive.

## Vær nøye og grundig med testing

For å sikre at testene dine er pålitelige og nyttige, er det viktig å være nøye og grundig når du skriver dem. Det betyr å implementere tester for så mange scenarioer som mulig, og å sørge for at de dekker alle hjørner og kantkase situasjoner. Det er også en god praksis å regelmessig kjøre testene dine og gjøre eventuelle nødvendige endringer i koden basert på feilene som oppdages.

## Se også

- [Hspec dokumentasjon](https://hspec.github.io/)

- [10 tips for å forbedre dine Haskell tester](https://www.fpcomplete.com/blog/10-tips-for-using-haskell-tests/)

- [Test-drevet utvikling i Haskell](https://skilldrick.co.uk/tdd-in-haskell/)

Ved å implementere god testing i Haskell-koden din, kan du forbedre kvaliteten på programvaren din og redusere risikoen for feil og bugs. Selv om det kan ta litt ekstra tid og krefter, vil det være verdt det i det lange løp. Lykke til med testing!