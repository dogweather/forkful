---
title:    "Haskell: Skriving av tester"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skrive tester?

Å skrive tester er en viktig del av utviklingsprosessen for enhver programvare. Det tillater programmerere å sikre at koden deres fungerer som forventet og reduserer risikoen for feil i produksjon. Ved å skrive tester kan man også enklere finne og fikse eventuelle feil, og det kan bidra til å forbedre kvaliteten på koden.

## Hvordan

Å skrive tester i Haskell er enkelt og effektivt. Det finnes ulike biblioteker og rammeverk tilgjengelig for å skrive tester, men vi skal fokusere på å bruke HUnit, et populært rammeverk for enhetstesting.

Først må du importere HUnit biblioteket øverst i koden din:

```Haskell
import Test.HUnit
```

Deretter kan du definere enhetstester ved å bruke funksjonen `TestList`. For eksempel, la oss teste en enkel funksjon som legger sammen to tall:

```Haskell
-- Funksjonen vi skal teste
sum :: Int -> Int -> Int
sum x y = x + y

-- Definisjon av enhetstesten
testSum :: Test
testSum = TestLabel "Test sum" (
    TestCase (assertEqual "1 + 2 skal være 3" 3 (sum 1 2))
)

-- Kjøre testen
main :: IO Counts
main = runTestTT $ TestList [testSum]
```

Her bruker vi funksjonen `assertEqual` for å sammenlikne den faktiske verdien av funksjonen vår med den forventede verdien, og sjekker om de er like. Dersom de ikke er det, vil testen feile.

For å kjøre testen kan vi bruke kommandoen `runTestTT` og teste alle definerte enhetstester ved å legge dem til i `TestList`.

## Dypdykk

Å implementere gode tester kan være en kunst i seg selv. Det er viktig å tenke på hva slags scenarier og verdier man vil teste for, og ikke bare fokusere på å dekke så mye kode som mulig. Det er også viktig å skrive tydelige testnavn, slik at man enkelt kan identifisere hva som blir testet.

En god praksis er å bruke et rammeverk som støtter "test-driven development" (TDD), hvor man skriver testene før man implementerer funksjonene. Dette hjelper til med å sikre at koden fungerer som den skal, og at den bare inneholder funksjonalitet som er nødvendig.

Det kan også være nyttig å bruke en form for kontinuerlig integrasjon (CI) for å automatisk kjøre alle tester hver gang kode blir endret. Dette kan gi en rask tilbakemelding om eventuelle feil, og tillater utviklere å identifisere og fikse dem raskt.

## Se også

Her er noen nyttige ressurser for å lære mer om å skrive tester i Haskell:

- [HUnit dokumentasjon](https://hackage.haskell.org/package/HUnit)
- [Haskell unit testing tutorial](https://www.tutorialspoint.com/haskell/haskell_testing.htm)
- [Test-driven development i Haskell](https://donsbot.wordpress.com/2007/10/28/test-driven-haskell/)