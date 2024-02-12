---
title:                "Umgang mit komplexen Zahlen"
aliases: - /de/java/working-with-complex-numbers.md
date:                  2024-01-26T04:42:07.865425-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Komplexe Zahlen erweitern die reelle Zahlengerade durch Hinzufügen einer imaginären Einheit, `i`, wobei `i^2 = -1` gilt. Sie sind von entscheidender Bedeutung in Bereichen wie Ingenieurwesen, Physik und fortgeschrittener Mathematik, wo sie Phänomene modellieren, die mit reellen Zahlen nicht darstellbar sind, wie elektrische Ströme und Signalverarbeitung.

## Wie:

Java unterstützt komplexe Zahlen nicht direkt, aber wir können unsere eigene Klasse erstellen oder eine Bibliothek verwenden. Hier ist ein schnelles Beispiel, wie man eine einfache `ComplexNumber`-Klasse erstellt und verwendet:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString, um komplexe Zahlen im Format a + bi anzuzeigen
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Schneller Test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Summe: " + c1.add(c2));
    }
}
```

Beispielausgabe für die Hauptmethode wird sein:

```
Summe: 3.0 + 7.0i
```

## Tiefergehend

Bevor es Hochsprachen wie Java gab, arbeiteten Programmierer direkt mit mathematischen Bibliotheken in Sprachen wie Fortran oder C, um komplexe Operationen zu verwalten. Das Konzept geht zurück auf das 16. Jahrhundert und wird Mathematikern wie Gerolamo Cardano und Rafael Bombelli zugeschrieben.

In Java ist `java.lang.Math` die Anlaufstelle für das Wesentliche, lässt aber komplexe Zahlen aus, wahrscheinlich weil nicht jeder Programmierer sie verwendet. Alternativen? Verwenden Sie Bibliotheken. Apache Commons Math bietet eine mit Methoden zur Manipulation vollgepackte `Complex`-Klasse. Hier ist jedoch der Vorteil, seine eigene zu erstellen: Leichtgewichtig, maßgeschneidert für Ihre genauen Bedürfnisse und ohne Bibliotheks-Overhead.

Ein wichtiges Detail: Achten Sie auf die Genauigkeit von Fließkommazahlen. Computer können einige Zahlen nicht exakt darstellen, was zu Rundungsfehlern führt. Bei wiederholten komplexen Operationen können sich diese Fehler anhäufen!

## Siehe auch

Für tiefergehende Einblicke und komplexere Operationen, siehe:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience's Complex class](http://jscience.org/)
- Oracles Tutorials über [Fließkommaarithmetik](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
