---
title:    "Javascript: Schreiben auf den Standardfehler"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Nachrichten an den Standardfehler ist ein wichtiger Bestandteil der Entwicklung in JavaScript. Durch die Verwendung von "console.error" können Entwickler Fehler und Warnungen in ihrem Code schnell und effizient identifizieren und beheben. Dies hilft dabei, die Qualität des Codes zu verbessern und mögliche Fehler zu vermeiden.

# Wie man es macht

Um eine Nachricht an den Standardfehler zu schreiben, verwenden wir die Methode "console.error" und übergeben ihr die Nachricht, die wir ausgeben möchten. Hier ist ein Beispiel:

```Javascript
console.error("Es ist ein Fehler aufgetreten!");
// Output: Es ist ein Fehler aufgetreten!
```

Wir können auch mehrere Nachrichten gleichzeitig an den Standardfehler senden, indem wir sie durch Kommas trennen:

```Javascript
console.error("Fehler beim Laden der Seite:", url);
// Output: Fehler beim Laden der Seite: www.example.com
```

# Tieferer Einblick

Das Schreiben von Nachrichten an den Standardfehler ist ein wichtiger Bestandteil der Fehlerbehebung in JavaScript. Durch die Verwendung von "console.error" können wir nicht nur Fehler und Warnungen ausgeben, sondern auch Variablen und Objekte in einer lesbaren Form anzeigen lassen:

```Javascript
const num1 = 10;
const num2 = 5;
console.error("Das Ergebnis von " + num1 + " geteilt durch " + num2 + " ist:", num1/num2);
// Output: Das Ergebnis von 10 geteilt durch 5 ist: 2
```

Außerdem können wir auch CSS-Styles auf die ausgegebenen Nachrichten anwenden, um wichtige Fehler hervorzuheben:

```Javascript
console.error("%cFehler: Seite nicht gefunden!", "color: red; font-size: 20px;");
// Output: Fehler: Seite nicht gefunden! (in rot und größerer Schrift)
```

# Siehe auch

- Die Dokumentation zu "console.error" von Mozilla Developer Network (https://developer.mozilla.org/de/docs/Web/API/Console/error)
- Ein ausführliches Tutorial zum Schreiben von Nachrichten an den Standardfehler auf Codecademy (https://www.codecademy.com/en/forum_questions/53f505b2fb7721e6b600195c)