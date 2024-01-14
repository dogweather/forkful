---
title:    "TypeScript: Ein String großschreiben"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Capitalizing einer Zeichenfolge kann nützlich sein, um beispielsweise Anzeigen oder Überschriften in einer lesbaren Form darzustellen.

## Wie geht das

```TypeScript
// Ein Beispiel einer Zeichenfolge, die wir capitalizen wollen
let stringToCapitalize: string = "hallo welt";

// Wir nutzen die eingebaute Methode "toUpperCase()" um die Zeichenfolge zu capitalizen
let capitalizedString: string = stringToCapitalize.toUpperCase();

// Das erwartete Ergebnis wäre: "HALLO WELT"
console.log(capitalizedString);
```

## Tief einsteigen

Das Capitalizing einer Zeichenfolge ist in TypeScript einfach mit der Verwendung der eingebauten Methode "toUpperCase()" zu erreichen. Diese Methode kann auch auf einzelne Buchstaben oder Wortteile angewendet werden, indem man die entsprechenden Indizes angibt.

Es gibt auch die Möglichkeit, eine benutzerdefinierte Funktion zu erstellen, die die Groß- und Kleinschreibung einer Zeichenfolge einheitlich formatiert. Dies kann besonders nützlich sein, wenn man mit Benutzereingaben arbeitet, bei denen die Groß- und Kleinschreibung nicht immer einheitlich ist.

## Siehe auch

- [Offizielles TypeScript-Handbuch zur Stringmanipulation](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Codebeispiele zur string-Manipulation in TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Weitere nützliche Informationen zur Groß- und Kleinschreibung in JavaScript und TypeScript](https://www.w3schools.com/jsref/jsref_tolowercase.asp)