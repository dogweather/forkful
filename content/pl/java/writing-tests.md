---
title:    "Java: Pisanie testów"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy? 

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki nim możemy upewnić się, że nasz kod działa poprawnie i nie powoduje błędów. Testy są również pomocne w utrzymaniu jakości i niezawodności naszej aplikacji. 

## Jak to zrobić?

Aby napisać testy w języku Java, możemy skorzystać z narzędzia JUnit. Poniżej przedstawiam prosty przykład testu jednostkowego wraz z oczekiwanym wynikiem: 

```Java
public class CalculatorTest {

    @Test
    public void addTest() {
        Calculator calculator = new Calculator();
        int result = calculator.add(2, 2);
        int expected = 4;
        assertEquals(expected, result);
    }
    
}
```

```Java
public class Calculator {

    public int add(int a, int b) {
        return a + b;
    }
    
}
```

W powyższym przykładzie tworzymy klasę `Calculator` zawierającą metodę `add`, a następnie w klasie `CalculatorTest` tworzymy test jednostkowy, który sprawdza, czy wynik dodawania jest poprawny. Po uruchomieniu testu, oczekujemy, że wynik będzie równy oczekiwanemu. W ten sposób możemy weryfikować poprawność działania naszych metod.

## Deep Dive

Pisanie testów wymaga znajomości różnych typów testów i narzędzi. Musimy mieć pewną wiedzę na temat testów jednostkowych, integracyjnych oraz interfejsów programistycznych (API). Dobrą praktyką jest również wykorzystywanie narzędzi do automatyzacji testów, takich jak Selenium czy Cucumber. 

Należy pamiętać, że testy powinny być pisane na początku procesu tworzenia oprogramowania, a nie na końcu jako ostatnia czynność. W ten sposób możemy uniknąć problemów z wykryciem błędów w późniejszych etapach.

## Zobacz również

- [Tutorial wideo - Jak napisać testy w języku Java](https://www.youtube.com/watch?v=KN3P0xQnEXE)
- [Dokumentacja JUnit](https://junit.org/junit5/docs/current/user-guide/)
- [Podstawy pisania testów w języku Java](https://www.baeldung.com/java-testing-tools)