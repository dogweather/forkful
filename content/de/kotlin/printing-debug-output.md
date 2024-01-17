---
title:                "Ausgabe von Debug-Informationen"
html_title:           "Kotlin: Ausgabe von Debug-Informationen"
simple_title:         "Ausgabe von Debug-Informationen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Drucken von Debug-Ausgaben ist ein Mittel, um während der Entwicklung eines Programms die verschiedenen Funktionen und Variablen zu überprüfen. Programmierer nutzen es, um Fehler zu finden und zu beheben sowie um das Programmverhalten zu verstehen.

## Wie geht's:

**Beispiel 1:** Einfaches Drucken von Variablen:
```Kotlin
val num = 5
println(num)
```
Ausgabe:
```
5
```

**Beispiel 2:** Verwendung von "string interpolation" für lesbare Debug-Ausgaben:
```Kotlin
val name = "Anna"
val age = 27
println("Name: $name, Alter: $age")
```
Ausgabe:
```
Name: Anna, Alter: 27
```

**Beispiel 3:** Debug-Ausgaben in komplexeren Funktionen:
```Kotlin
fun calculateSum(num1: Int, num2: Int): Int {
   println("Berechne Summe von $num1 und $num2")
   return num1 + num2
}
val sum = calculateSum(3, 5)
println("Summe: $sum")
```
Ausgabe:
```
Berechne Summe von 3 und 5
Summe: 8
```

## Detaillierte Informationen:

**Historischer Kontext:** Debug-Ausgaben haben in der Programmierung eine lange Tradition und waren schon in frühen Programmiersprachen wie Fortran und COBOL weit verbreitet. Durch die Entwicklung von leistungsfähigeren Debugger-Tools haben sich jedoch alternative Methoden zur Fehlerbehebung etabliert.

**Alternative Methoden:** Neben dem Drucken von Debug-Ausgaben können Programmierer auch Breakpoints setzen und den Code Schritt für Schritt durchgehen, um Fehler zu finden. Es gibt auch spezielle Tools und Frameworks, die bei der Fehlerbehebung helfen können.

**Implementierungsdetails:** In Kotlin können Debug-Ausgaben mit der Funktion `println()` und der Verwendung von "string interpolation" einfach und schnell realisiert werden. Es ist jedoch wichtig, im fertigen Programm alle Debug-Ausgaben zu entfernen, da sie sonst die Performance beeinträchtigen können.

## Siehe auch:

- [Debugging in Kotlin](https://kotlinlang.org/docs/reference/debugging.html)
- [Debugging Techniques](https://www.c-sharpcorner.com/UploadFile/248e18/debugging-techniques-in-vb-net/)
- [A Guide to Debugging](https://www.freecodecamp.org/news/a-guide-to-debugging-7fff44d7efa9/)