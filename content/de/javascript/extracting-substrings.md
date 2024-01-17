---
title:                "Extrahieren von Teilstrings"
html_title:           "Javascript: Extrahieren von Teilstrings"
simple_title:         "Extrahieren von Teilstrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

"## Was & Warum?"
Substring-Extraktion ist das Verfahren, bei dem Teilstrings aus einem vorhandenen String extrahiert werden. Programmierer nutzen dies häufig, um spezifische Teile eines Textes zu isolieren oder zu bearbeiten.

"## Anleitung:"
Das Erzeugen von Substrings in Javascript ist einfach und erfordert nur wenige Codezeilen. Hier sind zwei Beispiele:

```Javascript
let string = "Hallo Welt!";
console.log(string.substring(0, 5)); // Ausgabe: "Hallo"
console.log(string.substring(6, 11)); // Ausgabe: "Welt!"
```

Der obige Code erzeugt zwei Substrings aus dem ursprünglichen String "Hallo Welt!". Durch Angabe des Start- und Endpunktes können wir genau definieren, welcher Teil des ursprünglichen Strings extrahiert werden soll.

"## Tiefere Einblicke:"
Die Substring-Extraktion ist ein häufig verwendetes Verfahren in der Programmierung, aber woher stammt es überhaupt? Ursprünglich wurde dieses Konzept in der Programmiersprache "BASIC" implementiert und ist seitdem in vielen anderen Programmiersprachen, einschließlich Javascript, weit verbreitet.

Es gibt auch Alternativen zur Substring-Extraktion wie Regular Expressions, die ebenfalls zur Manipulation von Strings verwendet werden können. Allerdings ist die Verwendung von RegExp oft komplizierter als die einfache Substring-Extraktion und erfordert eine andere Syntax.

Bei der Implementierung der Substring-Extraktion in Javascript gibt es einige Dinge zu beachten. Zum Beispiel ist die Funktion "substring()" nicht in allen Browsern verfügbar und kann daher auf Probleme stoßen, wenn sie in älteren Versionen verwendet wird. In solchen Fällen ist es ratsam, die Funktion "slice()" zu verwenden, die ähnlich wie "substring()" funktioniert, aber in den meisten Browsern unterstützt wird.

"## Siehe auch:"
Für weitere Informationen über Substring-Extraktion und ihre Verwendung in anderen Programmiersprachen, schauen Sie sich diese nützlichen Quellen an:

- Mozilla Developer Network (MDN): Substring-Extraktion in Javascript: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- W3Schools.com: Substring-Extraktion in BASIC: https://www.w3schools.com/jsref/jsref_substring.asp