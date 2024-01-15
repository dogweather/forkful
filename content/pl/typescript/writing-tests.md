---
title:                "Pisanie testów"
html_title:           "TypeScript: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się napisać kod, który później przestał działać? Albo przeprowadzić zmiany w swoim projekcie, a następnie odkryć, że coś zupełnie nieprzewidywalnego przestało działać? Właśnie po to służą testy w TypeScript - aby uniknąć takich sytuacji i mieć pewność, że nasz kod działa zgodnie z oczekiwaniami.

## Jak to zrobić

Jako pierwsze powinniśmy zainstalować narzędzie `ts-test` za pomocą polecenia `npm install ts-test --save-dev`. Następnie należy utworzyć katalog `tests` w naszym projekcie, a w nim plik `example.spec.ts`. W tym pliku możemy napisać nasz test używając biblioteki `assert`:

```TypeScript
import assert from 'assert';

const sum = (a: number, b: number) => a + b;

describe("Testujemy funkcję sum", () => {
  it("Powinna zwracać wynik 5 dla wartości 2 i 3", () => {
    assert.strictEqual(sum(2, 3), 5);
  });

  it("Powinna zwracać wynik -10 dla wartości -7 i -3", () => {
    assert.strictEqual(sum(-7, -3), -10);
  });
});
```
Po uruchomieniu testów za pomocą `npm test`, powinniśmy otrzymać informację o sukcesie lub błędzie naszych testów.

## Głębszy zanurzenie

Testy w TypeScript pozwalają nam na przetestowanie prawie każdej części kodu - funkcji, metod, obiektów, a nawet interfejsów. Możemy wykorzystać różne biblioteki do asercji, takie jak `expect` lub `chai`, aby pisać bardziej czytelne i zrozumiałe testy.

Warto również pamiętać o odpowiednich nazwach naszych testów, aby łatwiej było zidentyfikować błędne części kodu. Dzięki testom możemy także tworzyć bardziej odporne aplikacje, gdyż w razie zmian lub dodania nowego kodu, testy pomogą nam szybko wykryć ewentualne problemy.

## Zobacz również
- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [Film "TypeScript Testing with Mocha and Chai"](https://www.youtube.com/watch?v=CFyNx70MYrE)
- [Artykuł "10 Reasons Why You Should be Using TypeScript in your Next Project"](https://www.sitepen.com/blog/2013/12/31/10-reasons-why-you-should-be-using-typescript/)