---
title:                "Gleam: Generieren von Zufallszahlen"
simple_title:         "Generieren von Zufallszahlen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals ein Programm geschrieben haben, bei dem Sie zufällige Zahlen benötigten, wissen Sie, wie frustrierend es sein kann, immer wieder dieselben Zahlen zu erhalten. Die Lösung? Die Verwendung einer Funktion zur Generierung von Zufallszahlen in Ihrer Gleam-Programmierung.

## Wie geht's

Gleam bietet eine Funktion namens `Float.random()` zum Generieren zufälliger Gleitkommazahlen zwischen 0 und 1. Sie können auch eine Untergrenze und Obergrenze einstellen, um Zahlen in einem bestimmten Bereich zu erhalten. Schauen wir uns ein Beispiel an:

```Gleam 
let result = Float.random(10, 20)
```

Dieses Beispiel würde eine Zufallszahl zwischen 10 und 20 generieren und sie der Variablen `Result` zuweisen. Sie können auch eine ganze Zahl anstelle einer Gleitkommazahl erhalten, indem Sie `Float` durch `Int` ersetzen. Hier ist ein weiteres Beispiel:

```Gleam
let result = Int.random(1, 10)
```

Dieses Beispiel würde eine zufällige ganze Zahl zwischen 1 und 10 generieren und sie der Variablen `Result` zuweisen. Sie können auch Ihre eigenen benutzerdefinierten Zahlen verwenden, indem Sie Beispiele aus der `Float.random()`-Funktion als Argumente verwenden. Zum Beispiel:

```Gleam
let result = Float.random(5.5, 11.2)
````

Dies würde eine zufällige Gleitkommazahl zwischen 5,5 und 11,2 generieren und sie der Variablen `Result` zuweisen.

## Tiefentauchgang

Wenn Sie sich fragen, wie Gleam Zufallszahlen generiert, verwendet es den `random()`-Algorithmus, der auf Mersenne-Twister basiert. Dies ist ein sehr effizienter Algorithmus, der eine große Anzahl an Zufallszahlen generieren kann, ohne dass sich Duplikate wiederholen. Es verwendet einen internen Zustand, um jederzeit eine neue zufällige Zahl zu generieren, was bedeutet, dass es nicht auf externe Faktoren wie die Systemzeit angewiesen ist. 

## Siehe auch

- Offizielle Gleam-Dokumentation zur `random()`-Funktion (https://gleam.run/documentation/std_lib/float.html#random)
- Gleam-Grundlagen für Einsteiger (https://dev.to/christopherbiscardi/getting-started-with-gleam-lang-13bc)
- Einblick in den Mersenne-Twister-Algorithmus (https://en.wikipedia.org/wiki/Mersenne_Twister)