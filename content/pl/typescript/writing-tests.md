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

## Co i dlaczego?

Pisanie testów to proces, w którym programiści tworzą specjalne fragmenty kodu, które mają na celu sprawdzenie, czy dany program działa poprawnie. Testowanie jest ważnym etapem w procesie tworzenia oprogramowania, ponieważ pomaga w wykrywaniu błędów i zapewnieniu, że aplikacja działa zgodnie z oczekiwaniami.

## Jak to zrobić:

```TypeScript 
function add(x: number, y: number) { 
  return x + y; 
}
 
console.log(add(5, 10));
// Output: 15
```

W tym przykładzie tworzymy funkcję "add", która przyjmuje dwa parametry typu "number" i zwraca ich sumę. Następnie wywołujemy tę funkcję z argumentami "5" i "10" i sprawdzamy, czy dostajemy oczekiwany wynik.

## Głębsze spojrzenie:

Pisanie testów jest częścią procesu TDD (Test Driven Development), który powstał w 2002 roku. Alternatywą dla tworzenia testów jest podejście BDD (Behavior Driven Development), wprowadzone w 2009 roku. Implementacja testów w TypeScript jest podobna do innych języków programowania, co oznacza, że możemy używać narzędzi takich jak Mocha lub Jest do tworzenia i uruchamiania testów.

## Zobacz także:

- [Wprowadzenie do Test Driven Development](https://www.agilealliance.org/glossary/tdd/)