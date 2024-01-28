---
title:                "Feilhåndtering"
date:                  2024-01-26T00:53:30.636622-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering betyr å skrive kode som forventer og håndterer at ting går galt. Programmører gjør dette for å gjøre programvaren robust, for å forhindre krasj og merkelig oppførsel.

## Hvordan:

Java bruker unntak (exceptions) til å håndtere feil. Du omslutter risikofylt kode med en `try`-blokk og fanger unntak med `catch`. Her er et enkelt eksempel:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Resultatet er: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Oops, kan ikke dele med null!");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

Output:
```
Oops, kan ikke dele med null!
```

## Dypdykk

Feilhåndtering i Java har utviklet seg. Tidlige dager hadde ikke unntak; programmerere sjekket feilkoder. Deretter introduserte Java try-catch-blokker, noe som tillot mer elegant feilhåndtering.

Alternativer til tradisjonelle `try-catch` inkluderer `try-with-resources` for automatisk lukking av ressurser og renere kode, introdusert i Java 7.

Detaljer i implementasjonen er viktige. For eksempel er det vanligvis dårlig praksis å fange `Exception` eller `Throwable`. Det er for bredt og kan skjule bugs du kanskje ikke er klar over. Hold deg til spesifikke unntak.

## Se Også

- De offisielle Oracle Java-tutorialene om unntak: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Javas `try-with-resources`-uttalelse dokumentasjon: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java av Joshua Bloch, for beste praksis om unntak.
