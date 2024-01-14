---
title:    "Haskell: Att skriva tester"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla mjukvara på ett effektivt och hållbart sätt. Genom att skriva tester kan man säkerställa att koden fungerar som den ska och undvika potentiella buggar och problem i framtiden.

## Hur man gör

För att skriva tester i Haskell använder man sig oftast av biblioteket HUnit. Det är ett lättanvänt verktyg som hjälper till att automatisera testprocessen. Nedan följer ett exempel på hur man kan skriva ett test för en funktion som beräknar summan av två tal:

```Haskell
import Test.HUnit

-- En funktion som beräknar summan av två tal
summa :: Int -> Int -> Int
summa a b = a + b

-- Tester för funktionen summa
testSumma1 = TestCase (assertEqual "Ska bli 10" 10 (summa 3 7))
testSumma2 = TestCase (assertEqual "Ska bli 0" 0 (summa (-3) 3))
testSumma3 = TestCase (assertEqual "Ska bli 5" 5 (summa 2 3))

-- En lista med alla tester
tests = TestList [testSumma1, testSumma2, testSumma3]

-- Kör alla tester och skriv ut resultatet
main = runTestTT tests
```

Genom att köra detta test kommer du att få utskriften "OK, 3 testcases" om alla tester lyckas. Om något test misslyckas kommer du att få en mer detaljerad förklaring om vad som gick fel.

## Djupdykning

Det finns många olika sätt att skriva tester på i Haskell, men det är viktigt att hålla sig till några grundläggande principer för att få ut mesta möjliga av sina tester. Här är några tips:

- Skriv test för varje funktion och se till att testa så många olika fall som möjligt (inklusive felaktiga invärden).
- Testa inte bara outputen av funktionen, utan också dess interna struktur. Detta kan hjälpa till att identifiera potentiella problem.
- Använd sig av QuickCheck för att generera slumpmässiga tester och öka testtäckningen.

## Se även

- [HUnit](https://hackage.haskell.org/package/HUnit) - Dokumentation för HUnit biblioteket.
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) - Dokumentation för QuickCheck biblioteket.
- [Test-Driven Development in Haskell: From Theory to Practice](https://www.codewars.com/books/test-driven-development-in-haskell-from-theory-to-practice) - En bok som fokuserar på TDD i Haskell och ger många bra exempel på hur man kan skriva tester.