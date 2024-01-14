---
title:    "Java: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Programmierer eine Zeichenkette in Kleinbuchstaben umwandeln möchten. Einer der häufigsten Gründe ist die Konsistenz bei der Überprüfung von Benutzereingaben in Formularen oder das Vergleichen von Texten, um sicherzustellen, dass sie gleich sind, unabhängig davon, ob die Groß- und Kleinschreibung unterschiedlich ist.

## Wie mache ich das?

Die Konvertierung einer Zeichenkette in Kleinbuchstaben ist in Java sehr einfach und erfordert nur eine einzige Methode: ```toLowerCase()```. Diese Methode nimmt die vorhandene Zeichenkette als Eingabe und gibt eine neue Zeichenkette in Kleinbuchstaben als Ausgabe zurück.

Hier ist ein Beispiel, wie dies in Java aussehen würde:

```
String text = "Programmieren ist toll!";
String lowerCaseText = text.toLowerCase();
System.out.println(lowerCaseText);
```

Die Ausgabe wäre: ```programmieren ist toll!```

## Tiefergehende Informationen

Es gibt einige Aspekte, die bei der Konvertierung einer Zeichenkette in Kleinbuchstaben zu beachten sind. Zum Beispiel, wie sich diese Methode auf Sonderzeichen oder diakritische Zeichen auswirkt. In der Dokumentation von Java können Sie mehr über die ```toLowerCase()``` Methode und ihre Funktionalität erfahren.

Es ist auch wichtig zu wissen, dass diese Methode immer eine neue Zeichenkette als Ausgabe erzeugt und die ursprüngliche Zeichenkette unverändert lässt. Wenn Sie also die ursprüngliche Zeichenkette ebenfalls in Kleinbuchstaben ändern möchten, müssen Sie dies manuell tun oder die neue Zeichenkette als Ersatz verwenden.

## Siehe auch

- [Java-Dokumentation zu toLower()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java-Tutorial zu String-Manipulation](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [w3schools Java String toLower() Beispiel](https://www.w3schools.com/java/ref_string_tolower.asp)