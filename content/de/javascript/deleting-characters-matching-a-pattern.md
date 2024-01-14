---
title:    "Javascript: Entfernen von Zeichen, die einem Muster entsprechen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufige Aufgabe beim Programmieren. Möglicherweise möchten Sie bestimmte Zeichen aus einer Zeichenkette entfernen oder in einer bestimmten Struktur nach einem bestimmten Muster suchen. Egal aus welchem Grund, das Löschen von Zeichen kann Ihnen dabei helfen, effizientere und präzisere Ergebnisse zu erzielen.

## Wie man es macht

Es gibt verschiedene Möglichkeiten, um Zeichen basierend auf einem bestimmten Muster zu löschen. Eine Möglichkeit ist die Verwendung der `replace()`-Methode in JavaScript. Diese Methode durchsucht eine Zeichenkette nach einem bestimmten Muster und ersetzt alle Übereinstimmungen mit einem leeren Zeichen. Zum Beispiel:

```Javascript
let string = "Hallo Welt!";

// Löscht alle Vokale aus der Zeichenkette
let neueZeichenkette = string.replace(/[aeiou]/ig, "");
console.log(neueZeichenkette); // "Hll Wlt!"
```

In diesem Beispiel wird die `replace()`-Methode verwendet, um alle Vokale in der Zeichenkette `string` zu löschen. Das Muster `/[aeiou]/ig` sucht nach allen Vokalen, unabhängig von Groß- und Kleinschreibung, und ersetzt sie durch ein leeres Zeichen. Die Option `i` sorgt dafür, dass auch Großbuchstaben gefunden werden, während `g` dafür sorgt, dass alle Übereinstimmungen ersetzt werden.

Eine weitere Möglichkeit ist die Verwendung der `filter()`-Methode. Diese Methode durchläuft jedes Element in einem Array und gibt nur die Elemente zurück, die einem bestimmten Muster entsprechen. Zum Beispiel:

```Javascript
let array = ["Apfel", "Banane", "Orange", "Melone"];

// Entfernt alle Elemente, die das Muster "a" enthalten
let neuesArray = array.filter(element => !element.includes("a"));
console.log(neuesArray); // ["Banane", "Melone"]
```

In diesem Beispiel wird die `filter()`-Methode verwendet, um alle Elemente im Array zu entfernen, die das Zeichen "a" enthalten. Die `includes()`-Methode prüft, ob ein Element das angegebene Zeichen enthält. Mit der Verwendung von `!` wird diese Aussage negiert, sodass nur Elemente zurückgegeben werden, die das Zeichen nicht enthalten.

## Tiefergehende Informationen

Es gibt noch viele weitere Möglichkeiten, um Zeichen basierend auf einem bestimmten Muster zu löschen. Sie können beispielsweise Reguläre Ausdrücke, auch bekannt als Regex, verwenden, um komplexere Muster zu suchen und zu ersetzen. Sie können auch verschiedene String-Methoden wie `slice()` oder `substring()` in Kombination mit bedingten Anweisungen verwenden, um gezielt Zeichen zu entfernen.

Es ist wichtig zu beachten, dass das Löschen von Zeichen basierend auf bestimmten Mustern eine mächtige Funktion ist, die jedoch auch sorgfältig eingesetzt werden sollte. Stellen Sie sicher, dass Sie das gewünschte Verhalten genau verstehen und testen Sie Ihre Lösungen gründlich, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Array.prototype.filter()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)