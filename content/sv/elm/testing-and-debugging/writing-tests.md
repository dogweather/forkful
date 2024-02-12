---
title:                "Skriva tester"
aliases:
- /sv/elm/writing-tests/
date:                  2024-02-03T19:30:40.961868-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Elm innebär att skapa testfall för att verifiera riktigheten av din Elm-kod, säkerställande att den fungerar som förväntat. Programmerare gör det för att fånga upp buggar tidigt, underlätta underhåll och förbättra kvaliteten och tillförlitligheten i deras applikationer.

## Hur man gör:

Elm använder paketet `elm-explorations/test` för att skriva enhetstester och fuzz-tester. Börja med att lägga till paketet i ditt projekt:

```elm
elm install elm-explorations/test
```

Skapa en testfil, säg `tests/ExampleTest.elm`, och importera testmodulerna. Här är ett enkelt test som verifierar funktionen `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "En enkel additionsfunktion"
        [ test "Att lägga till 2 och 3 ger 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

För att köra dina tester behöver du `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Detta kommer att kompilera dina tester och skriva ut resultaten i din terminal. För exemplet ovan ska utmatningen vara något i stil med:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

För ett mer komplext exempel, låt oss säga att du vill fuzz-testa `add`-funktionen för att säkerställa att den hanterar ett brett spektrum av heltalsinmatningar korrekt. Du skulle då modifiera din `ExampleTest.elm` som följer:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testar add med fuzzning"
        [ fuzz int "Fuzz-testar add med slumpmässiga heltal" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Kör `elm-test` igen för att se fuzz-testerna i aktion. Utmatningen kommer variera med slumpmässig inmatning men framgångsrika tester kommer att indikera inga misslyckanden:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

Dessa exempel visar hur man skriver och kör enkla enhets- och fuzz-tester i Elm, med paketet `elm-explorations/test`. Testning är en vital del av utvecklingsprocessen, som hjälper till att säkerställa att dina Elm-applikationer är tillförlitliga och bibehåller hög kvalitet.
