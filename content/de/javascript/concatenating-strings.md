---
title:                "Zeichenketten verknüpfen"
html_title:           "Javascript: Zeichenketten verknüpfen"
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Bei der Entwicklung von Anwendungen in Javascript ist es oft notwendig, verschiedene String-Werte miteinander zu kombinieren. Dies kann beispielsweise bei der Erstellung von dynamischen Anzeigeelementen oder bei der Verarbeitung von Nutzereingaben hilfreich sein. Im folgenden Artikel zeigen wir Ihnen, wie Sie Strings in Javascript einfach und effektiv verketten können.

# Wie es geht

Die Verkettung von Strings wird in Javascript durch den "+" Operator ermöglicht. Dieser wird verwendet, um zwei oder mehrere Strings zusammenzuführen. Hier ein einfaches Beispiel:

```Javascript
let name = "Max";
let greeting = "Hallo";
let message = greeting + " " + name;
console.log(message); //Ausgabe: Hallo Max
```

In diesem Beispiel werden die Variablen `greeting` und `name` kombiniert und in der Variable `message` gespeichert. Beachten Sie, dass zwischen den einzelnen Strings Leerzeichen eingefügt werden müssen, um ein korrektes Ergebnis zu erhalten.

Um mehr als zwei Strings zu verketten, können Sie den gleichen Ansatz verwenden:

```Javascript
let text = "Ich mag" + " " + "Javascript";
console.log(text); //Ausgabe: Ich mag Javascript
```

Eine andere Möglichkeit ist die Verwendung von Template-Strings, die es ermöglichen, Variablen direkt in einen String einzufügen. Hier ein Beispiel:

```Javascript
let city = "Berlin";
let message = `Willkommen in ${city}`;
console.log(message); //Ausgabe: Willkommen in Berlin
```

# Tiefergehende Informationen

Es ist wichtig zu beachten, dass Javascript bei der Verkettung von Strings automatisch in den Datentyp "String" konvertiert. Dies bedeutet, dass auch andere Datentypen wie Zahlen oder Booleans problemlos mit Strings kombiniert werden können. Hier ein Beispiel:

```Javascript
let age = 25;
let message = "Ich bin " + age + " Jahre alt";
console.log(message); //Ausgabe: Ich bin 25 Jahre alt
```

Zudem können Sie auch Methoden wie `concat()` oder `join()` verwenden, um Strings in Javascript zu verketten. Weitere Informationen zu diesen Methoden finden Sie in der offiziellen Javascript-Dokumentation.

# Siehe auch

- [Offizielle Javascript-Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript)
- [W3Schools - String Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN Web Docs - Template Strings](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/template_strings)