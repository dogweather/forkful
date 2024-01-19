---
title:                "Einen String großschreiben"
html_title:           "Javascript: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Im Grunde genommen ist Zeichenkettenvergrößerung (string capitalization) die Umwandlung der ersten Buchstaben eines Strings in Großbuchstaben. Programmierer nutzen das oft, wenn sie Daten präsentieren oder formatieren möchten.

## So geht's:

In JavaScript können Sie den ersten Buchstaben eines Strings auf verschiedene Weisen großschreiben. Hier sind ein paar Beispiele:

```Javascript
// Methode 1: Mit 'slice' und 'toUpperCase'
let text = 'hallo welt';
let capitalizedText = text.charAt(0).toUpperCase() + text.slice(1);
console.log(capitalizedText);  // Ergibt: 'Hallo welt'

// Methode 2: Mit 'split' und 'map'
let text2 = 'hallo welt';
let capitalizedText2 = text2.split(' ').map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(' ');
console.log(capitalizedText2); // Ergibt: 'Hallo Welt'
```

## Tiefer Eintauchen:

Die Methode der Zeichenkettenvergrößerung stammt von der Programmiersprache C und ist in vielen modernen Programmiersprachen vorhanden. Alternativen sind in der Bibliothek der jeweiligen Sprachen zu finden, z.B. die Lodash-Bibliothek in JavaScript bietet eine Funktion namens _.capitalize().

Die Verwendung von 'charAt(0).toUpperCase()' statt 'toUpperCase.charAt(0)' ist wichtig, da der letztere bei einigen Compilern zu Fehlern führen kann, insbesondere wenn der String leer ist.

Verstehen Sie auch, dass die grundsätzliche Methodik zur Großschreibung von Strings den ganzen String nicht ändert, bis Sie das Ergebnis in einer neuen Variablen speichern oder die ursprüngliche Variable damit überschreiben. JavaScript-Strings sind unveränderlich, deshalb funktioniert das so.

## Siehe auch:

- [MDN Web Docs: String.prototype.charAt()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs: String.prototype.toUpperCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Lodash-Bibliothek: _.capitalize()](https://lodash.com/docs/#capitalize)