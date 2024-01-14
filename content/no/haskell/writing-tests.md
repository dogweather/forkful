---
title:    "Haskell: Skrive tester"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en god programmerer. Det hjelper deg med å sikre deg at koden din fungerer som den skal, og det gjør det lettere å fange feil hvis noe går galt senere.

## Hvordan

For å skrive tester i Haskell, kan du bruke rammeverket Hspec. Her er et eksempel på hvordan du kan teste en enkel funksjon som adderer to tall:

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "adds two numbers correctly" $ do
      add 5 3 `shouldBe` 8
```

I dette eksempelet bruker vi `describe` for å definere en beskrivelse av testene våre, og `it` for å spesifisere hva testen skal gjøre. Vi bruker også `shouldBe` for å sjekke om det forventede resultatet er det samme som det faktiske resultatet.

For å kjøre testene, kan du kjøre følgende kommando i terminalen: `stack test`.

Når testene er kjørt, vil du se en oversikt over hvor mange tester som ble kjørt, og om de bestod eller feilet.

## Dypdykk

Når du skriver tester, er det viktig å tenke på hvilke situasjoner koden din kan havne i, og å teste for alle mulige scenarier. Du bør også sørge for å skrive rene og lesbare tester slik at det blir enkelt å forstå hva som testes og hva som eventuelt feiler.

Det kan også være lurt å ha et godt test-dekningsverktøy som kan hjelpe deg med å identifisere hvilke deler av koden som ikke er dekket av tester. Dette kan hjelpe deg med å identifisere områder som trenger mer testing.

## Se også
- [Hspec dokumentasjon](https://hspec.github.io/)
- [HaskellTesting - en samling av ressurser for testing i Haskell](https://wiki.haskell.org/Testing)