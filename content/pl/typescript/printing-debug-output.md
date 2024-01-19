---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Drukowanie debug outputu to jedna z najprostszych metod śledzenia działania naszego kodu. Umożliwia nam to obserwację zmiennych, sprawdzanie wywołania funkcji i identyfikację miejsc, w których coś idzie nie tak.

## Jak to zrobić:

W TypeScript komunikaty służące do debugowania są najczęściej wyświetlane za pomocą `console.log()`. Możemy je wywołać z dowolnej części naszego kodu, aby sprawdzić stan konkretnej zmiennej lub działanie funkcji.

```TypeScript
let zmienna: number = 5;
console.log(zmienna);
function test() {
    console.log("Funkcja testowa została wywołana");
}
test();
```

Dla powyższego kodu otrzymamy w konsoli poniższy output:

```Output
5
Funkcja testowa została wywołana
```

## Głębsze spojrzenie:

Historically, debugging could sometimes be a complex process, especially in lower level languages that didn't offer built-in debugging tools. However, modern languages like TypeScript have made it much easier, integrating debugging tools directly into the language's standard library.

Historically, debugging sometimes could be a complex process, especially in lower level languages that didn't offer built-in debugging tools. However, modern languages like TypeScript have made it much easier, integrating debugging tools directly into the language's standard library.

Niektóre alternatywy dla `console.log` to `console.info()`, `console.warn()` i `console.error()`, które pozwalają na bardziej specyficzne adnotacje dotyczące naszych komunikatów debugujących.

Co ciekawe, `console.log` w rzeczywistości nie jest funkcją TypeScriptu, ale jest udostępnione przez środowisko wykonawcze JavaScript. TypeScript jedynie typuje tę funkcję jako `Console.log`.

## Zobacz również:

Dla bardziej zaawansowanych funkcji debugowania, warto zaznajomić się z narzędziami debuggera wbudowanymi do TypeScriptu: [Debugging TypeScript](https://www.typescriptlang.org/docs/handbook/debugging.html)

Można również zapoznać się z dokumentacją `console`: [Console API reference](https://developer.mozilla.org/en-US/docs/Web/API/Console)