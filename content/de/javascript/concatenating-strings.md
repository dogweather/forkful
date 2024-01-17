---
title:                "Verkettung von Zeichenketten"
html_title:           "Javascript: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

Was & Warum?
Concatenating strings, oder im Deutschen "Zeichenkettenverknüpfung", ist eine häufig verwendete Technik in der Programmierung, um mehrere Zeichenketten (Wörter, Sätze, Ziffern usw.) zu einer einzigen Zeichenkette zu verbinden. Programmierer nutzen diese Technik, um dynamische Inhalte zu erzeugen, zum Beispiel um personalisierte Nachrichten oder dynamisch generierte URLs zu erstellen.

Wie funktioniert es?
👉 Code-Beispiel: 
```Javascript
let str1 = "Hallo";
let str2 = "Welt";
let result = str1 + " " + str2;
console.log(result);
```

👉 Ausgabe:
```Text
Hallo Welt
```

In diesem Beispiel werden die beiden Zeichenketten "Hallo" und "Welt" mit dem Pluszeichen (+) miteinander verbunden. Der resultierende Output ist die zusammengesetzte Zeichenkette "Hallo Welt". Beachte, dass zwischen den beiden Zeichenketten ein Leerzeichen eingefügt wurde, um ein besseres Leseverständnis zu ermöglichen.

Tiefer einladen
👉 Historischer Kontext:
Die Verknüpfung von Zeichenketten gibt es schon seit den Anfängen der Programmierung. In frühen Programmiersprachen wie BASIC und COBOL wurde die Verkettung oft mit dem "&"-Symbol verwendet. Mit der Einführung von JavaScript wurden jedoch das Pluszeichen (+) und die Methode .join() bevorzugte Methoden für die Zeichenkettenverknüpfung.

👉 Alternativen:
Eine alternative Methode zur Verknüpfung von Zeichenketten ist die Verwendung von Template Literals, auch bekannt als String Interpolation. Diese Methode ermöglicht es, Variablen direkt in einen Text einzufügen, ohne das Pluszeichen verwenden zu müssen.

👉 Details zur Implementierung:
JavaScript behandelt Zeichenketten als Objekte, wodurch sie verschiedene Methoden wie .concat(), .slice() und .substring() unterstützen. Diese können ebenfalls zur Zeichenkettenverknüpfung verwendet werden, obwohl das Pluszeichen (+) immer noch die gängigste Methode ist.

Siehe auch
Hier sind einige hilfreiche Links zum Thema Zeichenkettenverknüpfung:

- [MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [Stack Overflow](https://stackoverflow.com/questions/18004/how-can-i-concatenate-strings-in-javascript)