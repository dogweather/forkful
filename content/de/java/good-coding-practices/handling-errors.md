---
date: 2024-01-26 00:53:19.778134-07:00
description: "Wie geht das: Java verwendet Ausnahmen (Exceptions) zur Fehlerbehandlung.\
  \ Sie umgeben riskanten Code mit einem `try`-Block und fangen Ausnahmen mit\u2026"
lastmod: '2024-03-13T22:44:53.770005-06:00'
model: gpt-4-1106-preview
summary: Java verwendet Ausnahmen (Exceptions) zur Fehlerbehandlung.
title: Fehlerbehandlung
weight: 16
---

## Wie geht das:
Java verwendet Ausnahmen (Exceptions) zur Fehlerbehandlung. Sie umgeben riskanten Code mit einem `try`-Block und fangen Ausnahmen mit `catch`. Hier ist ein einfaches Beispiel:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Ergebnis ist: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Hoppla, kann nicht durch Null teilen!");
        }
    }

    private static int divide(int zaehler, int nenner) {
        return zaehler / nenner;
    }
}
```

Ausgabe:
```
Hoppla, kann nicht durch Null teilen!
```

## Tiefere Einblicke
Die Fehlerbehandlung in Java hat sich weiterentwickelt. In den Anfängen gab es keine Ausnahmen; Programmierer prüften Fehlercodes. Dann führte Java try-catch-Blöcke ein, was eine elegantere Fehlerbehandlung ermöglichte.

Alternativen zum traditionellen `try-catch` umfassen `try-with-resources` für das automatische Schließen von Ressourcen und klareren Code, eingeführt in Java 7.

Implementierungsdetails sind wichtig. Zum Beispiel ist das Fangen von `Exception` oder `Throwable` normalerweise eine schlechte Praxis. Es ist zu allgemein und verbirgt Fehler, von denen man sich nicht bewusst sein könnte. Halten Sie sich an spezifische Ausnahmen.

## Siehe auch
- Die offiziellen Oracle Java-Tutorials zu Ausnahmen: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Java's `try-with-resources` Anweisungsdokumentation: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java von Joshua Bloch, für Best Practices zu Ausnahmen.
