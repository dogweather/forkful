---
title:                "Einen String großschreiben"
html_title:           "TypeScript: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was und warum?

Das Kapitalisieren eines Strings bedeutet, dass wir den ersten Buchstaben eines Worts oder aller Wörter im String zu Großbuchstaben machen. Das wird oft gemacht, um Texte formal aussehen zu lassen oder bestimmte Wörter hervorzuheben.

## So machst du's:

Unten findest du ein Beispiel, wie du das in TypeScript machen kannst:

```TypeScript
function capitalizeFirstLetter(string: string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

let beispiel = 'hallo Welt';
console.log(capitalizeFirstLetter(beispiel));
```

Wenn du das in einem TypeScript-Projekt ausführst, wird es 'Hallo Welt' ausgeben.

Für die Kapitalisierung aller Wörter könnte man diese Funktion so anpassen:

```TypeScript
function capitalizeAllWords(string: string) {
    return string.split(' ').map(function(word) {
        return word.charAt(0).toUpperCase() + word.slice(1);
    }).join(' ');
}

let beispiel2 = 'hallo schöne welt';
console.log(capitalizeAllWords(beispiel2));
```

Das wird dann 'Hallo Schöne Welt' ausgeben.

## Vertiefung:

Die Praxis, den first Buchstaben in Großbuchstaben zu verwandeln, ist ein Überbleibsel aus der Zeit, als Texte noch mit einer Schreibmaschine oder per Hand geschrieben wurden. Bei diesen alten Techniken war es schwierig, einzelne Wörter hervorzuheben, daher wurden wichtige Wörter oft großgeschrieben.

Es gibt andere Möglichkeiten, dieses Ziel zu erreichen. Statt `.charAt(0).toUpperCase() + .slice(1)` könntest du eine reguläre Expression verwenden:

```TypeScript
function capitalizeFirstLetterRegEx(string: string) {
    return string.replace(/^(.)/, function(match, group1) {
        return group1.toUpperCase();
    });
}
```

Beachte, dass diese Techniken nicht für alle Schriftsysteme funktionieren. Viele nicht-lateinische Alphabete haben keine Groß-/Kleinschreibung.

## Weiterführende Links:

- [MDN Web Docs zu String.prototype.toUpperCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs zu String.prototype.charAt()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [StackOverflow Diskussion zum Kapitalisieren von Strings in TypeScript](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)