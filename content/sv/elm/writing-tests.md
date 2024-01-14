---
title:                "Elm: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Även om det kan verka tråkigt och tidskrävande, är det väldigt viktigt att skriva tester när man programmerar i Elm. Tester hjälper till att säkerställa att koden fungerar som den ska och förhindrar att felaktiga ändringar görs senare. Det kan också ge bättre förståelse för koden och underlätta vid framtida utveckling.

## Hur man skriver tester

För att skriva tester i Elm behöver man först importera modulen `Test` och allt som behövs för testningen, såsom modellerna som ska testas och eventuella funktioner.

```Elm
import Test
import ExampleModule exposing (..)
```

Sedan kan man definiera ett `Test.suite` där man anger namn och en lista med `Test.test` för varje test man vill köra. Inuti varje test kan man utnyttja `Test.equal` eller andra liknande funktioner för att kontrollera om ett visst uttryck är lika med förväntat resultat.

```Elm
Test.suite "Exempeltest"
    [ Test.test "Kontrollera add funktionen"
        (\_ -> 
            let 
                result = add 2 3
            in
                Test.equal result 5
        )
    ]
```

För att köra testerna kan man sedan använda terminalen och kommandot `elm-test` eller verktyg som "elm-test-rs" för att få en mer visuell representation av testerna. Det är också rekommenderat att köra testerna regelbundet, gärna vid varje ny kodändring.

## Djupdykning

För att skriva effektiva tester är det viktigt att förstå hur Elm fungerar. Det finns till exempel några viktiga koncept att ha koll på:

- Modularitet: Det är en bra idé att dela upp testerna i moduler som motsvarar modulerna i den faktiska koden.
- Mocking: Ibland kan det vara användbart att använda `Test.succeed` för att simulera ett positivt testfall eller `Test.fail` för att testa felaktiga förhållanden.
- Generering: Med hjälp av biblioteket `elm-test-generators` kan man generera slumpmässiga testfall för att testa kod på ett mer omfattande sätt.

Se till att läsa på om dessa och andra koncept för att skapa mer sofistikerade tester.

## Se även

- [Elm Test documention](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm Test genererators](https://package.elm-lang.org/packages/elm-community/elm-test-extra/latest/Generators)
- [Elm Test exempelprojekt](https://github.com/elm-explorations/test/tree/latest/example) för att se testning i praktiken