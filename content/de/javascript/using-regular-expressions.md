---
title:                "Javascript: Reguläre Ausdrücke verwenden"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum 

Wenn Sie schon einmal Javascript programmiert haben, haben Sie wahrscheinlich schon von regulären Ausdrücken oder auch Regex gehört. Aber warum sollte man sich überhaupt damit beschäftigen? Reguläre Ausdrücke ermöglichen es uns, komplexe Muster in Zeichenketten zu finden und zu verarbeiten. Dies kann uns helfen, unsere Programmieraufgaben effizienter zu lösen.

# Wie geht's

Die Verwendung von regulären Ausdrücken in Javascript ist relativ einfach. Zunächst müssen wir den `RegExp` Konstruktor nutzen, um ein neues reguläres Ausdruckobjekt zu erstellen. Zum Beispiel, um nach einer bestimmten Anzahl von Ziffern in einer Zeichenkette zu suchen, können wir folgenden Code verwenden:

```Javascript
let regex = new RegExp('\\d{3}-\\d{4}');
let result = regex.exec("123-4567");
console.log(result[0]); // Output: 123-4567
```

Hier haben wir ein reguläres Ausdruckobjekt erstellt, welches nach drei Ziffern gefolgt von einem Bindestrich und dann vier Ziffern sucht. Mit der `exec()` Methode suchen wir in der Zeichenkette "123-4567" nach diesem Muster und erhalten als Ergebnis die gefundene Zeichenkette, die dem regulären Ausdruck entspricht. 

Natürlich gibt es noch viele weitere Optionen und Methoden, um reguläre Ausdrücke in Javascript zu nutzen. Eine ausführliche Dokumentation finden Sie in der [MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp).

# Tiefergehende Informationen

Reguläre Ausdrücke ermöglichen es uns, Muster in Zeichenketten zu finden und zu verarbeiten. Aber in welchen Situationen können diese nützlich sein? Ein Anwendungsbeispiel ist die Validierung von Benutzereingaben in Formularen. Mit regulären Ausdrücken können wir die Eingaben auf bestimmte Formate überprüfen, wie zum Beispiel eine E-Mail Adresse oder eine Telefonnummer. Dadurch können wir sicherstellen, dass die Daten in einer korrekten Form vorliegen, bevor sie weiterverarbeitet werden.

Eine weitere Einsatzmöglichkeit ist das Suchen und Ersetzen von Texten in größeren Dateien. Stellen Sie sich vor, Sie müssen in einer riesigen Datei mehrere Hundert URLs durch andere ersetzen. Mit regulären Ausdrücken lässt sich dies schnell und effizient erledigen, indem man nur das Muster des gewünschten Textes eingibt und dann die Ersetzungsfunktion nutzt.

In der Tiefe gibt es noch viele weitere Aspekte zu beachten, wie zum Beispiel die Verwendung von speziellen Zeichen und Meta-Zeichen, um noch genauere Muster zu erstellen. Es lohnt sich, sich ausreichend damit zu beschäftigen, um das volle Potenzial von regulären Ausdrücken auszuschöpfen.

# Siehe auch

- [MDN Web Docs - RegExp](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Regexr - Online regulärer Ausdruck Tester](https://regexr.com/)
- [RegExPal - Reguläre Ausdrücke visuell erklärt](https://www.regexpal.com/)