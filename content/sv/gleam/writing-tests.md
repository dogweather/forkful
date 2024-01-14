---
title:                "Gleam: Respåiredåförattteskriver: Skapa tester"
simple_title:         "Respåiredåförattteskriver: Skapa tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingsprocessen för alla programmerare. Det hjälper till att säkerställa att koden fungerar korrekt och minskar risken för buggar och fel i produktion.

## Hur man skriver tester i Gleam

Det första steget i att skriva tester i Gleam är att importera modulen `gleam/test`. Detta ger tillgång till funktioner som `test` och `assert` som används för att skapa och utföra tester.

```Gleam
import gleam/test
```

För att börja skriva tester, använd funktionen `test` och ge den ett namn och en funktion som ska köras som test. Inuti denna funktion kan `assert` användas för att kontrollera om det förväntade resultatet stämmer överens med det faktiska resultatet.

```Gleam
test "Summera två tal" {
  assert add(2, 3) == 5
}
```

Om testet inte passerar kommer det att visa ett felmeddelande tillsammans med information om vilket test som misslyckades och varför.

```bash
Test failed: Summera två tal : Expected 5 but got 6
```

Det är också möjligt att använda flera `assert` i ett test för att kontrollera flera olika scenarion.

```Gleam
test "Kontrollera om tal är jämnt eller udda" {
  assert 2 |> is_even
  assert 3 |> is_odd
}
```

## Djupdykning

Att skriva tests är en viktig del av test-driven development (TDD). Det innebär att skriva tester för att specificera hur koden ska fungera innan själva implementationen sker. Detta hjälper till att tydliggöra vilka funktioner som behöver skapas och hur de ska bete sig.

I Gleam är det också möjligt att skapa tester för privata funktioner genom att importera modulen `gleam/test/private`. Detta gör att man kan testa privata funktioner utan att behöva exponera dem i det offentliga gränssnittet.

När man skriver tester i Gleam är det också viktigt att fokusera på att testa funktionaliteten och inte implementationen. Detta gör det möjligt att refaktorera och förbättra koden utan att behöva oroa sig för att testerna inte längre kommer att fungera.

## Se även

- [Officiell dokumentation för Gleam Tester](https://gleam.run/book/tour/testing.html)
- [Gleam-testing-repo på GitHub](https://github.com/gleam-lang/gleam-testing)
- [Test-Driven Development för Elm-utvecklare](https://medium.com/@swiftcare/tdd-for-elm-developers-4f35389ad99d) (på engelska)