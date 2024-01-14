---
title:    "Arduino: Strings verbinden"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Arduino-Programmierung ist die Verkettung von Zeichenketten (Strings) eine häufige Aufgabe. Es ermöglicht Ihnen, mehrere Zeichenketten zu einer einzelnen zusammenzufügen, was sehr nützlich sein kann, wenn Sie zum Beispiel Daten aus verschiedenen Sensoren kombinieren möchten. In diesem Blog-Beitrag werden wir uns ansehen, wie man Zeichenketten in Arduino verketten kann und warum es wichtig ist, dies zu wissen.

## Wie geht man vor?

Um Zeichenketten in Arduino zu verketten, verwenden wir den Operator "+" zwischen den einzelnen Zeichenketten. Schauen wir uns ein Beispiel an:

```Arduino
String name = "Max";
String greeting = "Hallo " + name;
Serial.println(greeting);
```

Die Ausgabe dieses Codes wäre "Hallo Max". Wie Sie sehen können, haben wir die Zeichenkette "Max" an die Zeichenkette "Hallo " angehängt, um eine neue Zeichenkette zu bilden.

Dieser Prozess kann auch mit mehr als zwei Zeichenketten durchgeführt werden. Schauen wir uns ein weiteres Beispiel an:

```Arduino
String noun = "Apfel";
String verb = "essen";
String sentence = "Ich mag " + noun + " " + verb + "!";
Serial.println(sentence);
```

Die Ausgabe dieses Codes wäre "Ich mag Apfel essen!". Wie Sie sehen können, können wir mehrere Zeichenketten in einer einzigen Zeichenkette verketten, indem wir einfach den "+" Operator verwenden.

## Tiefer Einblick

Das Verketten von Zeichenketten kann auch mit anderen Datentypen wie Zahlen und Variablen durchgeführt werden. Schauen wir uns ein Beispiel an:

```Arduino
int num1 = 5;
int num2 = 10;
String result = "Die Summe von " + String(num1) + " und " + String(num2) + " ist " + String(num1 + num2) + "!";
Serial.println(result);
```

Die Ausgabe dieses Codes wäre "Die Summe von 5 und 10 ist 15!". Hier haben wir die Zahlen in Zeichenketten umgewandelt, um sie mit anderen Zeichenketten zu verketten.

Es ist auch wichtig zu beachten, dass die Verkettung von Zeichenketten in Arduino nicht so effizient ist wie in anderen Programmiersprachen wie C++. Wenn Sie viele Zeichenketten verketten müssen, könnte dies die Leistung des Programms beeinträchtigen. In diesem Fall sollten Sie eine andere Technik, wie z.B. das Verwenden von Zeichenarrays, in Betracht ziehen.

## Siehe auch

- [Arduino-Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/concatenation/)
- [Tutorial: Verkettung von Strings in Arduino](https://www.hackster.io/Aritro/string-concatenation-in-arduino-c0fd8f)

Vielen Dank, dass Sie unseren Blog-Beitrag über das Verketten von Zeichenketten in Arduino gelesen haben. Wir hoffen, dass er Ihnen geholfen hat, ein besseres Verständnis dafür zu bekommen, wie man diese häufige Aufgabe in Ihren zukünftigen Projekten umsetzen kann. Bleiben Sie dran für weitere nützliche Arduino-Tutorials und Tipps!