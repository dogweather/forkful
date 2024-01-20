---
title:                "Pisanie testów"
html_title:           "Javascript: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów jest procesem polegającym na tworzeniu specjalnych skryptów w celu sprawdzenia poprawności kodu w trakcie jego tworzenia. Jest to ważna praktyka w świecie programowania, ponieważ pozwala ona uniknąć błędów i ustawić zagwarantować, że nasz kod jest niezawodny.

## Jak to zrobić:

Aby napisać testy w Javascript, używamy biblioteki o nazwie "Jest", która zapewnia nam narzędzia i funkcje do tworzenia testów. Poniżej znajdują się przykłady kodu i wyników dla testu funkcji ```add()```, która dodaje dwie liczby:

```
// importujemy metody "test" i "expect" z biblioteki 'jest'
const { test, expect } = require('jest'); 

// tworzymy funkcję do testowania - "add"
function add(a, b) {
  return a + b;
}

// wywołujemy funkcję "test" do przetestowania funkcji "add"
test('dodawanie działa poprawnie', () => {
  expect(add(2, 3)).toBe(5); // oczekujemy, że wynik będzie równy 5
  expect(add(-10, 10)).toBe(0); // oczekujemy, że wynik będzie równy 0
  expect(add('2', '3')).toBe(5); // oczekujemy, że wynik będzie równy 5
  // uwzględniamy też przypadki, w których mogą wystąpić błędy
  expect(add('2', null)).toBe(NaN); // oczekujemy, że wynik będzie równy NaN (Not a Number)
});
```

Wynik testów powinien wyglądać następująco:

```
PASS  __tests__/add.test.js
 ✓ dodawanie działa poprawnie (2ms)

Test Suites: 1 passed, 1 total
Tests: 1 passed, 1 total
Snapshots: 0 total
```

## Głębszy wgląd:

Pisanie testów w programowaniu jest praktyką, która pojawiła się wraz z rozwojem technologii Agile i TDD (Test Driven Development). Jest to podejście, w którym testy pisane są przed napisaniem właściwego kodu, a programista koncentruje się na tym, aby przejść wszystkie testy i tylko wtedy implementuje funkcjonalność.

Alternatywnym podejściem jest BDD (Behavior Driven Development), w którym testy pisane są w sposób bardziej zrozumiały dla nieprogramistów, korzystając z języka zbliżonego do języka naturalnego.

Implementacja testów może różnić się w zależności od biblioteki, ale zazwyczaj praktyka ta polega na tworzeniu funkcji do testowania i wywoływaniu ich przy użyciu narzędzi dostarczonych przez bibliotekę testową.

## Zobacz też:

- [Dokumentacja biblioteki "Jest"](https://jestjs.io/docs/en/getting-started) - informacje o metodach i funkcjach dostępnych w bibliotece