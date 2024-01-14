---
title:                "TypeScript: String großschreiben"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man in der Programmierung eine Zeichenfolge großschreiben würde. Eine mögliche Funktion könnte zum Beispiel sein, dass die Benutzeroberfläche einer Anwendung bestimmte Texte in Großbuchstaben anzeigen muss. Eine andere Möglichkeit wäre, dass man bestimmte Daten für die Datenbank speichern muss und dabei eine konsistente Schreibweise sicherstellen möchte.

## Wie geht das?

Eine Möglichkeit, eine Zeichenfolge in TypeScript großzuschreiben, ist die Verwendung der `toUpperCase()` Methode. Diese Methode wandelt alle Buchstaben in der Zeichenfolge in Großbuchstaben um und gibt eine neue Zeichenfolge zurück. 

```TypeScript
let string = "Hallo Welt";
let capitalizedString = string.toUpperCase();

console.log(capitalizedString); // Ausgabe: "HALLO WELT"
```

Man kann auch eine eigene Funktion schreiben, die jeden Buchstaben in der Zeichenfolge einzeln durchgeht und mithilfe des Character Codes in den entsprechenden Großbuchstaben umwandelt. Zum Beispiel könnte man die Funktion `capitalize()` folgendermaßen definieren:

```TypeScript
function capitalize(string: string): string {
    let capitalizedString = '';

    for (let i = 0; i < string.length; i++) {
        let charCode = string.charCodeAt(i); // Character Code des aktuellen Buchstabens
        if (charCode >= 97 && charCode <= 122) { // Buchstaben von 'a' bis 'z'
            // Wandelt den Buchstaben in den entsprechenden Großbuchstaben um
            capitalizedString += String.fromCharCode(charCode - 32);
        } else {
            capitalizedString += string[i]; // Fügt Zeichen unverändert hinzu
        }
    }

    return capitalizedString;
}

console.log(capitalize("Hallo Welt")); // Ausgabe: "HALLO WELT"
```

## Tiefer Einblick

In TypeScript ist eine Zeichenfolge ein Array von Unicode-Zeichen. Bei der Größe oder beim Inhalt einer Zeichenfolge kann man sich also nicht auf die Länge oder die im Code sichtbaren Zeichen verlassen. Um sicherzustellen, dass alle Buchstaben in der Zeichenfolge großgeschrieben werden, müssen also alle Unicode-Zeichen in Großbuchstaben umgewandelt werden.

Ein interessanter Punkt ist, dass die `toUpperCase()` Methode nicht nur für die englischen Buchstaben funktioniert, sondern auch für die Zeichen anderer Sprachen. So würde zum Beispiel die Zeichenfolge "résumé" nach der Großschreibung als "RÉSUMÉ" ausgegeben werden, da die Methode die Regeln der entsprechenden Sprache für Groß- und Kleinschreibung anwendet.

## Siehe auch

- [TypeScript Strings](https://www.typescriptlang.org/docs/handbook/2/strings.html)
- [Unicode Zeichen und der Character-Code](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)