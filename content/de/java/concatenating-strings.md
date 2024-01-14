---
title:                "Java: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren in Java müssen wir oft Strings miteinander verbinden, um komplexe Ausgaben zu erstellen. Die Verkettung von Strings bietet eine flexible Möglichkeit, Texte und Variablen zu kombinieren, um ein gewünschtes Ergebnis zu erzielen.

## Wie geht man vor?

Um Strings in Java zu verketten, müssen wir die "+"-Operator verwenden. Dieser Operator kann verwendet werden, um zwei oder mehr Strings miteinander zu verbinden. Schauen wir uns ein Beispiel an:

```Java
String str1 = "Hallo ";
String str2 = "Welt!";
String combinedStr = str1 + str2;

System.out.println(combinedStr);
```

In diesem Beispiel definieren wir zwei separate Strings "str1" und "str2" und verwenden dann den "+"-Operator, um sie zu verketten. Der kombinierte String wird dann in der Variablen "combinedStr" gespeichert und schließlich mit der Methode "println" ausgegeben. Das Ergebnis wäre:

```
Hallo Welt!
```

Dies ist nur ein einfaches Beispiel, aber die Verkettung von Strings kann auch mit unterschiedlichen Datentypen wie Zahlen oder Booleans funktionieren. Hier ist ein weiteres Beispiel:

```Java
String name = "Alice";
int age = 25;
String message = name + " ist " + age + " Jahre alt.";

System.out.println(message);
```

Das Ergebnis hier wäre:

```
Alice ist 25 Jahre alt.
```

## Tiefeneinblick

Bei der Verkettung von Strings ist es wichtig, darauf zu achten, dass die Reihenfolge der Elemente richtig ist. Strings werden wie von links nach rechts gelesen, daher ist es wichtig, die Reihenfolge der Verkettung entsprechend zu planen. Ebenso ist es wichtig, die Datentypen der Werte, die miteinander verknüpft werden sollen, zu berücksichtigen, um Fehler in der Ausführung zu vermeiden.

Ein weiterer wichtiger Aspekt ist die Verwendung von Methoden wie "concat()", um Strings zu verketten. Diese Methode bietet die gleiche Funktionalität wie der "+"-Operator, ist aber in einigen Fällen möglicherweise effizienter.

Zusammenfassend bietet die Verkettung von Strings eine flexible Möglichkeit, Texte und Variablen miteinander zu kombinieren und komplexe Ausgaben zu erstellen.

## Siehe auch

* [Java String Concatenation - GeeksForGeeks](https://www.geeksforgeeks.org/java-string-concatenation/)
* [Java String Concatenation: Plus Operator vs. concat() Method - DZone](https://dzone.com/articles/java-string-concatenation-plus-operator-vs-concat) 
* [Java String Concatenation Tutorial - Java2s](http://www.java2s.com/Tutorial/Java/0040__Data-Type/ConcatAStringWhenYouConvertOtherObjectToAString.htm)