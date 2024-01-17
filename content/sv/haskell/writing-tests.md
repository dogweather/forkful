---
title:                "Att skriva tester"
html_title:           "Haskell: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att skriva tester är en viktig del av programmering, det är helt enkelt att skriva kod för att kontrollera att vår kod fungerar som den ska. Det är ett sätt för oss att försäkra oss om att vår kod fortsätter att fungera även efter vi har gjort ändringar.

# Hur gör man?
För att skriva tester i Haskell, behöver vi ett testramverk som heter HUnit. Detta ramverk låter oss definiera tester i form av testfall och sedan köra dessa för att se om de passerar eller misslyckas.

Ett exempel på hur man skriver ett testfall med HUnit i Haskell:

```Haskell
import Test.HUnit

-- testfall som kontrollerar att summan av 1 och 2 är lika med 3
sumTest = TestCase (assertEqual "Sum should be 3" 3 (1+2))

-- lista med alla testfall
tests = TestList [TestLabel "sumTest" sumTest]

-- kör testen och se resultatet
main = runTestTT tests
```
Exempel på output:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

# Djupdykning
Att skriva tester har funnits sedan programmeringens tidiga år, men det var inte förrän på 2000-talet som det blev en mer strukturerad och organiserad process. Tidigare användes ofta manuella tester där en person kontrollerade koden, men det kunde bli långsamt och tidskrävande.

Ett alternativ till HUnit är QuickCheck, ett testramverk som använder slumpmässiga värden för att testa vår kod. Det är ett bra sätt att upptäcka buggar och gränsfall som vi kanske inte hade tänkt på.

En viktig del av att skriva tester är att försöka täcka så många olika scenarier som möjligt för att få en så pålitlig kod som möjligt. Detta kan ibland vara en utmaning, men det hjälper oss att förbättra vår kod och undvika problem i framtiden.

# Se även
- Officiell dokumentation för HUnit: https://hackage.haskell.org/package/HUnit
- Officiell dokumentation för QuickCheck: https://hackage.haskell.org/package/QuickCheck