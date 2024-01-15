---
title:                "Verbinden von Zeichenfolgen"
html_title:           "Gleam: Verbinden von Zeichenfolgen"
simple_title:         "Verbinden von Zeichenfolgen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Verkettung von Strings ist eine häufige Aufgabe bei der Entwicklung von Software und kann dabei helfen, Texte dynamisch zu erstellen und zu formatieren. Es ist eine grundlegende Fähigkeit, die jeder Programmierer beherrschen sollte.

## Wie es funktioniert

Die Verkettung von Strings bezieht sich auf die Zusammenführung von mehreren Textelementen zu einem einzigen String. In Gleam wird dies mit dem "+" Operator erreicht, der zwei Strings miteinander verbindet.

```Gleam
let name = "Max"
let greeting = "Hallo " + name
```

Dieses Beispiel würde "Hallo Max" als Ergebnis ausgeben. Es ist auch möglich, mehrere Strings nacheinander zu verketten, um längere Texte zu erstellen.

```Gleam
let sentence = "Mein Name ist " + name + " und ich bin " + age + " Jahre alt."
```

Dies würde "Mein Name ist Max und ich bin 30 Jahre alt." als Ergebnis liefern, wenn die Variable "age" den Wert "30" hat.

## Tiefes Eintauchen

Bei der Verkettung von Strings ist es wichtig, auf die Reihenfolge der einzelnen Elemente zu achten. Wenn zum Beispiel eine Zahl mit einem String verknüpft wird, muss sie zuerst in einen String umgewandelt werden, da Gleam keine implizite Konvertierung durchführt.

```Gleam
let count = 5
let message = "Die Anzahl der Elemente beträgt: " + String.from_int(count)
// Ergebnis: "Die Anzahl der Elemente beträgt: 5"
```

Es ist auch möglich, Variablen oder Funktionen innerhalb der Verkettung zu verwenden.

```Gleam
let number = 10
let formatted = "Das doppelte von " + String.from_int(number) + " ist " + String.from_int(double(number))

// Verwendung der 'double' Funktion
fn double(n: Int) {
    n * 2
}
```

Ein weiterer wichtiger Aspekt ist die Verwendung von Leerzeichen. Beim Verketten von Strings werden die einzelnen Elemente direkt aneinander gereiht, daher ist es wichtig, Leerzeichen innerhalb der Textelemente oder manuell einzufügen, um die gewünschte Formatierung zu erhalten.

## Siehe auch

- Dokumentation zu Strings in Gleam: https://gleam.run/book/tour/strings.html
- Gutes Beispielprojekt, das Strings verwendet: https://github.com/gleam-lang/example-projects/tree/master/echo