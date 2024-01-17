---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Javascript: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Konvertierung eines Strings in Kleinbuchstaben bezieht sich auf die Umwandlung aller Buchstaben eines Textes in ihre klein geschriebenen Formen. Programmierer nutzen dies oft, um sicherzustellen, dass Eingabedaten einheitlich verarbeitet werden oder um Vergleiche zwischen verschiedenen Strings durchzuführen.

## So geht's:
```Javascript
const str = "Hallo, Welt!";
console.log(str.toLowerCase()); // gibt "hallo, welt!" aus
```
Hier wird die eingebaute Funktion ```toLowerCase()``` verwendet, um den String "Hallo, Welt!" in die kleingeschriebene Form "hallo, welt!" umzuwandeln. Diese Funktion kann auf jedem String ausgeführt werden und ist in den meisten gängigen Programmiersprachen verfügbar.

## Tieferer Einblick:
Die Verwendung von Kleinbuchstaben hat sich aus der Notwendigkeit entwickelt, Texte in alphanumerischer Form zu speichern und zu verarbeiten, da frühe Computersysteme nur Großbuchstaben unterstützten. Alternativ zur Verwendung von ```toLowerCase()``` können Programmierer auch die Funktion ```String.prototype.toLocaleLowerCase()``` verwenden, die möglicherweise bessere Unterstützung für Unicode-Zeichen bietet. Es ist auch wichtig, zu beachten, dass die Umwandlung in Kleinbuchstaben Sprachspezifika berücksichtigen muss, da einige Sprachen wie das Türkische Sonderregeln für die Transformation von Buchstaben haben.

## Siehe auch:
- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Web Docs: String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)