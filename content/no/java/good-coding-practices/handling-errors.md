---
date: 2024-01-26 00:53:30.636622-07:00
description: "Feilh\xE5ndtering betyr \xE5 skrive kode som forventer og h\xE5ndterer\
  \ at ting g\xE5r galt. Programm\xF8rer gj\xF8r dette for \xE5 gj\xF8re programvaren\
  \ robust, for \xE5 forhindre\u2026"
lastmod: '2024-03-11T00:14:14.215972-06:00'
model: gpt-4-1106-preview
summary: "Feilh\xE5ndtering betyr \xE5 skrive kode som forventer og h\xE5ndterer at\
  \ ting g\xE5r galt. Programm\xF8rer gj\xF8r dette for \xE5 gj\xF8re programvaren\
  \ robust, for \xE5 forhindre\u2026"
title: "Feilh\xE5ndtering"
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
