---
title:    "Javascript: Substrings extrahieren"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

#
## Warum

In der Welt der Programmierung ist es oft notwendig, einen Teil eines Textes zu isolieren und damit zu arbeiten. Wenn Sie jemals versucht haben, einen Teil eines Strings (Zeichenkette) aus einem Text in Javascript zu extrahieren, sind Sie sicherlich auf die Methode substring() gestoßen. Diese Methode ermöglicht es uns, einen Teil eines Strings zu extrahieren, basierend auf einem Start- und Endindex. Aber warum sollten wir diese Methode verwenden?

Die Verwendung von substrings kann nützlich sein, um bestimmte Daten aus einem längeren Text zu extrahieren, z.B. eine Telefonnummer aus einem Textfeld in einem Formular. Mit der substring-Methode können wir auch verschiedene Teile eines Strings kombinieren, um einen neuen Text zu erstellen.

## Wie man substrings extrahiert

Mit Javascript ist es sehr einfach, einen Substring aus einem String zu extrahieren. Dazu verwenden wir die Methode substring() und geben die gewünschten Start- und Endindizes innerhalb der Klammern an. Hier ist ein einfaches Beispiel:

```Javascript
var text = "Dies ist ein Beispieltext.";
var substring = text.substring(5, 13);
console.log(substring); // Ergebnis: "ist ein B"
```

In diesem Beispiel haben wir den Substring von Index 5 bis Index 13 extrahiert und ihn in der Variablen "substring" gespeichert. Wir können auch negative Indizes verwenden, um von hinten an den String zu zählen. Zum Beispiel würde der Index -1 den letzten Buchstaben des Strings auswählen.

Es gibt noch eine weitere Methode, die wir verwenden können, um Substrings zu extrahieren - slice(). Diese Methode funktioniert ähnlich wie substring(), hat aber einige unterschiedliche Verhaltensweisen, wenn negative Indizes verwendet werden.

## Eine tiefere Erklärung

Beide Methoden, substring() und slice(), schneiden einen Teil eines Strings aus und geben ihn zurück. Der Hauptunterschied liegt darin, dass slice() in der Lage ist, negative Indizes zu verarbeiten und auch verwendet wird, um Arrays zu schneiden. Wenn wir nur einen Index angeben, wird die gesamte Zeichenkette ab diesem Index bis zum Ende zurückgegeben.

Eine weitere wichtige Sache zu beachten ist, dass sowohl substring() als auch slice() den Endindex nicht einschließen. Zum Beispiel würde die Angabe von 0 als Startindex und 3 als Endindex in beiden Methoden die ersten 3 Zeichen auswählen, aber nicht das vierte Zeichen.

## Siehe auch

- [substring() Methode in der MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [slice() Methode in der MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)
- [Javascript String-Methoden in der MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/#methoden_der_String_instanzen)