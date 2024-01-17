---
title:                "String großschreiben"
html_title:           "TypeScript: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

"## Was & Warum?"
Die Kapitalisierung eines Strings bedeutet, jeden Buchstaben in einem Wort in Großbuchstaben umzuwandeln. Programmer nutzen dies, um die Lesbarkeit von Variablen oder Ausgaben zu verbessern und die Konsistenz im Code zu wahren.

"## Wie geht's?"
Ein Beispiel in TypeScript:

```TypeScript
const name = "max";
const capitalizedName = name.toUpperCase();
console.log(capitalizedName); // outputs "MAX"
```

"## Tiefgehende Informationen"
Die Kapitalisierung von Strings hat in der Geschichte der Programmierung ihre Wurzeln in der ASCII-Kodierung, die nur Großbuchstaben unterstützt. Mit der Entwicklung von Unicode und UTF-8 steht Programmierern heute eine größere Auswahl an Zeichen und Buchstaben zur Verfügung.

Eine Alternative zur direkten Anwendung der `toUpperCase()` Funktion ist die Verwendung von String Interpolation, um das erste Zeichen eines String in einen Großbuchstaben zu verwandeln. Dies kann nützlich sein, wenn der restliche String in Kleinbuchstaben bleiben soll.

Die Implementierung der `toUpperCase()` Funktion in TypeScript folgt dem Unicode Standard, der die entsprechenden Codes für Großbuchstaben in der Kodierung definiert.

"## Siehe auch"
[MDN web docs - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)