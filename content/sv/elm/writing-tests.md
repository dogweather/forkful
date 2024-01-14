---
title:    "Elm: Skriva tester"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-tests.md"
---

{{< edit_this_page >}}

# Varför
Att skriva tester är ett viktigt steg i utvecklingen av Elm-program. Tester hjälper till att säkerställa att koden fungerar som den ska och att eventuella buggar hittas och åtgärdas innan de når produktion.

# Hur man gör det
För att skriva tester i Elm behöver du använda biblioteket Elm-test. Först lägger du till detta i din `elm.json` fil under "dependencies" sektionen.
```
elm install elm-explainer/elm-test -y
```

Sedan skapar du en ny fil för dina tester, till exempel `MyTests.elm`, och importerar biblioteket. Därefter kan du skriva dina tester med hjälp av funktioner som `expectEqual` och `test` inuti ett `describe` block.
```
import Expect exposing (expectEqual)
import Test exposing (describe, test)

myAdder : Int -> Int -> Int
myAdder x y =
    x + y

describe "Testing My Adder function" 
    [ 
        test "Adding 2 and 3 should equal 5" 
            (expectEqual (myAdder 2 3) 5), 
        test "Adding 5 and 7 should equal 12" 
            (expectEqual (myAdder 5 7) 12) 
    ]
```

Du kan också använda `fuzz` för att generera slumpmässiga värden att testa din kod med.
```
fuzz (tuple int int) "Testar MyAdder med slumpmässiga nummer" 
    (\(x, y) -> expectEqual (myAdder x y) (x + y))
```

När du är nöjd med dina tester kan du köra dem genom att köra kommandot `elm-test` i Terminalen. Om alla tester passerar, kommer du att se ett grönt meddelande, annars kommer du att få en lista över testerna som misslyckades och vad det förväntade värdet var.

# Djupdykning
När du skriver tester är det viktigt att täcka så många olika fall som möjligt. Det är också en bra idé att regelbundet gå tillbaka och uppdatera eller lägga till nya tester när koden har ändrats. Detta hjälper dig att undvika eventuella buggar och säkerställa att din kod fungerar som det är tänkt.

Ett annat tips är att använda `expectFail` för att testa felaktiga värden eller beteenden. Detta hjälper dig att fånga och åtgärda fel innan de påverkar din produktionskod.

# Se också
- [Elm-test dokumentation](https://package.elm-lang.org/packages/elm-explainer/elm-test/latest/)
- [En enkel guide till testning i Elm](https://dev.to/rtfeldman/a-beginner-s-guide-to-testing-in-elm-3oij)
- [Exempel på testning i Elm](https://medium.com/elm-shorts/test-yet-another-comma-separated-lib-a722b2f84a5c)