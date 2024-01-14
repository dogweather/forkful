---
title:                "Clojure: Ein String großschreiben"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man in der Programmierung oft aufgefordert wird, einen String in Großbuchstaben zu konvertieren. Zum Beispiel bei der Eingabe von Benutzerdaten, bei der Validierung von Passwörtern oder bei der Formatierung von Ausgabemeldungen. Egal aus welchem Grund, die Fähigkeit, Strings zu groß zu schreiben, ist eine wichtige Fähigkeit, die jeder Programmierer beherrschen sollte. 

## Wie man es macht

Die Lösung dieses Problems ist in Clojure relativ einfach, da die Programmiersprache eine eingebaute Funktion bereitstellt, um Strings in Großbuchstaben zu konvertieren.

Um einen String in Großbuchstaben zu konvertieren, müssen wir die Funktion `clojure.string/upper-case` verwenden. Diese Funktion nimmt einen String als Eingabe und gibt den gleichen String in Großbuchstaben zurück. Schauen wir uns ein Beispiel an:

```Clojure
(clojure.string/upper-case "hallo Welt")  ;=> "HALLO WELT"
```

Wie in diesem Beispiel zu sehen ist, wird der String "hallo Welt" in Großbuchstaben konvertiert. Dies gilt unabhängig von der ursprünglichen Schreibweise des Strings.

## Tiefentauchen

Nun wissen wir, wie man Strings in Großbuchstaben konvertieren kann, aber wie genau funktioniert die `clojure.string/upper-case` Funktion hinter den Kulissen? 

Diese Funktion verwendet intern die Java-Methode `toUpperCase()`, die einen String in Großbuchstaben umwandelt. Einer der Vorteile von Clojure ist, dass es einfach ist, in die zugrunde liegende Java-Schicht einzutauchen, um zu verstehen, wie die Funktionen arbeiten.

## Siehe auch

- [Clojure Docs zu String Funktionen](https://clojuredocs.org/clojure.string)
- [Java String Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())