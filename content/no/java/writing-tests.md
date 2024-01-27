---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester betyr å lage kode som sjekker at annen kode fungerer som forventet. Programmerere gjør dette for å fange feil tidlig, sikre kodekvalitet og forenkle vedlikehold.

## How to:
Vi bruker JUnit 5 for eksempelene. Her er en enkel test for å sjekke om en metode som legger til to tall virker:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void whenAddTwoNumbersThenCorrectResult() {
        Calculator calculator = new Calculator();
        assertEquals(4, calculator.add(2, 2), "2 + 2 skal bli 4");
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```
Kjør dette, og du får passert test hvis summene er riktige.

## Deep Dive
JUnit, opprettet av Erich Gamma og Kent Beck på 90-tallet, er standard for Java testrammeverk. Alternativer inkluderer TestNG og Spock. Når du skriver tester, er det viktig å fokusere på testdekning og å teste både suksess- og feilscenarioer.

## See Also
- JUnit 5 Brukerveiledning: https://junit.org/junit5/docs/current/user-guide/
- Maven repository for JUnit 5: https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-api
- Clean Code av Robert C. Martin, kapittel om testing: https://www.oreilly.com/library/view/clean-code-a/9780136083238/
