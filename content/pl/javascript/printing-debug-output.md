---
title:                "Drukowanie wyjścia debugowania"
html_title:           "Javascript: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W druku wyjściowym debugowania polega na wyświetlaniu informacji o kodzie, takich jak wartości lub funkcje, aby pomóc programistom w zrozumieniu co się dzieje w trakcie działania programu. Jest to szczególnie przydatne podczas debugowania błędów, gdyż pozwala zidentyfikować miejsce i przyczynę problemu.

## Jak to zrobić:
```Javascript
console.log("Wartość zmiennej x to: " + x);
```
W powyższym przykładzie używamy funkcji `console.log()` do drukowania wartości zmiennej `x` w konsoli przeglądarki. Możemy również wyświetlić inne informacje, takie jak tekst czy wynik działania funkcji.

## Głęboki wyjazd:
Printowanie wyjścia debugowania nie jest nowym konceptem. Zostało zapoczątkowane przez programistów, którzy potrzebowali szybkiego sposobu na sprawdzenie wartości zmiennych w trakcie pisania kodu. Alternatywnym sposobem na debugowanie może być stosowanie narzędzi programistycznych, takich jak debugger, ale wydruk wyjścia debugowania nadal jest popularnym i użytecznym sposobem w wielu przypadkach.

## Zobacz również:
- [Wprowadzenie do debugowania JavaScript](https://developer.mozilla.org/pl/docs/Learn/JavaScript/First_steps/What_went_wrong)
- [Console API w JavaScript](https://developer.mozilla.org/pl/docs/Web/API/Console)
- [5 przydatnych metod debuggingu w JavaScript](https://medium.com/javascript-in-plain-english/5-przydatnych-metod-debuggingu-w-javascript-c42454eb0673)