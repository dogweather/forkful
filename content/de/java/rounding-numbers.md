---
title:                "Zahlen runden"
date:                  2024-01-26T03:44:55.168734-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zahlen runden bedeutet, sie an einen bestimmten Genauigkeitsgrad anzupassen. Programmierer tun dies, um Zahlen für die Lesbarkeit zu vereinfachen, bestimmte Spezifikationen zu erfüllen oder um sicherzustellen, dass Berechnungen innerhalb bestimmter Grenzen passen, wie z.B. Genauigkeitsfehler bei Gleitkommaarithmetik zu vermeiden.

## Wie geht das:
Java bietet mehrere Möglichkeiten, Zahlen zu runden. Hier ist eine schnelle Demonstration mit `Math.round()`, `BigDecimal` und `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Verwendung von Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Ausgabe: 123

        // Verwendung von BigDecimal für mehr Kontrolle
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Ausgabe: 123.46

        // Verwendung von DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Ausgabe: 123.46
    }
}
```

## Vertiefung
Historisch gesehen war das Runden von Zahlen essentiell für analoge Berechnungen und hat sich für digitale Rechenoperationen wegen Effizienz und Genauigkeit durchgesetzt. Rundungsfehler, wie die bei Gleitkommaarithmetik, demonstrieren, dass dies keine triviale Angelegenheit ist – sie können kumulativ Berechnungen in beispielsweise der Luft- und Raumfahrt und Finanzanwendungen durcheinanderbringen.

Über `Math.round()` hinaus haben Sie `BigDecimal`, das Ihnen feinere Kontrolle über die Skalierung und den Rundungsmodus gibt, und `DecimalFormat`, wenn Sie Zahlen als Teil der Formatierung von Textausgaben runden müssen. Alternativen zum Runden umfassen die Verfahren Floor, Ceiling und Truncating, die verschiedene Arten sind, mit Präzision umzugehen und typischerweise durch verschiedene `Math`-Methoden behandelt werden.

Je nach Anwendungsfall kann die Rundungsstrategie variieren. Zum Beispiel ist `BigDecimal` die erste Wahl für Finanzberechnungen, wo Präzision kritisch ist. Im Gegensatz dazu ist `Math.round()` eine schnelle Methode für allgemeine Operationen, bei denen man weniger wählerisch bezüglich des Rundungsmodus ist.

## Siehe auch
- [Oracles Java Math-Dokumentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE-Standard für Gleitkommaarithmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat-Klasse in Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
