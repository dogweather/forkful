---
title:                "Pisanie testów"
html_title:           "Java: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów polega na tworzeniu kodu, który sprawdza, czy nasz program działa poprawnie. Programiści robią to w celu upewnienia się, że ich kod działa prawidłowo i zapobieganiu błędom w przyszłości.

## Jak to zrobić:

Aby napisać test w Javie, musimy użyć klas z pakietu ```org.junit```. Najpierw musimy zainicjować klasę testową używając adnotacji ```@Test```, a następnie napisać kod, który sprawdza czy nasza metoda zwraca oczekiwany wynik. Na przykład:

```
@Test
public void testAddition() {
  Calculator calc = new Calculator();
  int result = calc.add(2, 3);
  assertEquals(5, result);
}
```

Output będzie wyglądał następująco:

```
OK (1 test)
```

## Głębsze zanurzenie:

Pisanie testów ma swoje korzenie w metodyce programowania zwanej Test Driven Development (TDD), w której najpierw piszemy testy, a potem dopiero kod. Alternatywą dla testów jednostkowych jest testowanie funkcjonalne, gdzie sprawdzamy zachowanie całego systemu jako całości. W implementacji testów, warto zwrócić uwagę na dostępne narzędzia, takie jak JUnit czy TestNG.

## Zobacz też:

- Oficjalna dokumentacja JUnit: https://junit.org/junit5/docs/current/user-guide/
- Przykładowe testy w Javie: https://github.com/junit-team/junit5-samples