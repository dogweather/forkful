---
title:                "Att skriva tester."
html_title:           "Gleam: Att skriva tester."
simple_title:         "Att skriva tester."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Writing tests, or test-driven development, is the practice of writing automated tests for your code before writing the code itself. This ensures that your code is functioning as expected and avoids unexpected bugs. It also serves as documentation for your code and allows for easier maintenance and refactoring in the future.

## Hur man:
Följande kodblock visar hur man kan skriva tester i Gleam:

```Gleam
fn add(a, b) {
  a + b
}

test "add adds two numbers" {
  expect(add(1, 2)) |> to_equal(3)
}
```

Outputen vid körning av testet blir:

```
✓ add adds two numbers
```

## Djupdykning:
Test-driven development kommer från den agila utvecklingsmetodiken och har funnits sedan tidigt 2000-tal. Alternativ till Gleam för att skriva tester är till exempel ExUnit för Elixir eller Jest för JavaScript. I Gleam använder man sig av biblioteket Gleam Expect som tillhandahåller funktioner för att skriva och köra tester.

## Se även:
- [Gleam Expect documentation](https://github.com/gleam-lang/expect)
- [Test-driven development explained](https://www.agilealliance.org/glossary/tdd/)
- [Alternative testing frameworks](https://www.slant.co/options/24563/alternatives/~elixir-exunit-alternatives)