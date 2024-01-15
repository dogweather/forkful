---
title:                "Skriva tester"
html_title:           "Gleam: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Ibland kan det kännas som att det tar längre tid att skriva tester än att faktiskt skriva koden. Så varför ska man göra det? Jo, för att tester gör din kod mer tillförlitlig och hjälper dig att hitta buggar tidigt.

## Så här gör du
Det första du behöver göra är att installera Gleam, antingen manuellt eller genom ett pakethanterare som Homebrew på macOS. När det är klart kan du börja skriva dina tester. Här är ett exempel på en simpel funktion och tillhörande test:

```Gleam
fn sum(a, b) {
  a + b
}

test "sum function" {
  assert sum(2, 3) == 5
}
```

Du kan köra testet genom att köra kommandot `gleam test` i terminalen. Du bör få utskriften `1 passed (0 ignored) in 0ms`.

## Djupdykning
När du börjar skriva fler tester och behöver organisera dem kan du använda `module` och `suite` för att skapa olika grupper av tester. Detta gör det lättare att hitta och köra specifika tester. Här är ett exempel på hur det kan se ut:

```Gleam
module math {
  suite "sum function" {
    test "sum of two numbers" {
      assert sum(2, 3) == 5
    }

    test "sum of negative numbers" {
      assert sum(-2, -3) == -5
    }
  }
}
```

För att köra alla tester i denna modul kan du använda `gleam test math`. Detta kommer att köra båda testerna och du bör få utskriften `2 passed (0 ignored) in 1ms`.

## Se också
- [Gleam dokumentation](https://hexdocs.pm/gleam/getting-started.html)
- [En enkel guide till enhetstestning med Gleam](https://blog.frankel.ch/u-tdd-gleam/) (på engelska)
- [Kurs om testdriven utveckling med Gleam](https://www.udemy.com/course/functional-programming-test-driven-development/) (på engelska)