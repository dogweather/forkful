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

## Dlaczego

Testowanie jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki testom, deweloperzy mogą wczesniej wykrywać błędy i uniknąć problemów w późniejszych etapach projektu. W ten sposób, pisanie testów znacznie zwiększa jakość produktu i pozwala zaoszczędzić czas i pieniądze.

## Jak to zrobić

Pisanie testów w języku Java jest stosunkowo proste i wymaga znajomości kilku podstawowych koncepcji. Poniżej znajdują się przykłady kodów zawierających testy i ich wynik.

```java
// Przykładowa klasa, którą będziemy testować
public class Calculator {
    public int add(int a, int b){
        return a + b;
    }
}

// Test metody add() z wykorzystaniem biblioteki JUnit
import static org.junit.Assert.assertEquals;

@Test
public void testAdd() {
    Calculator calculator = new Calculator();
    assertEquals(5, calculator.add(2, 3));
}
```
Powstanie nowego obiektu klasy Calculator i wywołanie metody add() z dwoma argumentami 2 i 3 zwróci wartość 5, co jest potwierdzeniem poprawności działania metody.

Możemy również użyć biblioteki Mockito do tworzenia mocków i weryfikowania zachowań obiektów w naszych testach. Przykład kodu wykorzystującego tę bibliotekę wyglądałby następująco:

```java
// Przykładowa klasa, którą będziemy testować
public class MessageService {
    public void sendMessage(String message){
        System.out.println(message);
    }
}

// Test metody sendMessage() z wykorzystaniem biblioteki Mockito
import static org.mockito.Mockito.*;

@Test
public void testSendMessage() {
    MessageService messageService = mock(MessageService.class);
    messageService.sendMessage("Hello World");
    verify(messageService).sendMessage("Hello World");
}
```
Tworzymy tutaj mock klasy MessageService, wywołujemy na nim metodę sendMessage() z argumentem "Hello World" i sprawdzamy czy metoda ta została wywołana z tym samym argumentem.

## Pogłębione informacje

Pisanie testów w języku Java pozwala na lepsze zrozumienie kodu i jego funkcjonowania. Dzięki testom możemy upewnić się, że nasze metody działają zgodnie z oczekiwaniami i nie powodują przypadkowych błędów. Warto również pamiętać o odpowiednim nazewnictwie testów, tak aby były one czytelne i łatwe w utrzymaniu.

Istnieje wiele bibliotek przeznaczonych do testowania w języku Java, takich jak JUnit, Mockito czy AssertJ, które ułatwiają tworzenie i uruchamianie testów. Warto zapoznać się z nimi i wybrać tę, która najlepiej pasuje do naszych potrzeb.

## Zobacz także

[10 podstawowych sposobów pisania testów w Javie](https://www.pluralsight.com/blog/software-development/writing-clean-unit-tests-10-tips)

[Testowanie jednostkowe w języku Java - poradnik dla początkujących](https://www.toptal.com/developers/blog/junit-testing-tutorial-for-beginners)

[Oficjalna dokumentacja JUnit](https://junit.org/junit5/docs/current/user-guide/)