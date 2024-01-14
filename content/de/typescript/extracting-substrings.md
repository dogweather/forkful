---
title:    "TypeScript: Substrings extrahieren."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilsätzen aus einer Zeichenfolge ist ein nützliches Werkzeug, um bestimmte Informationen aus einem Text herauszufiltern. Es kann beispielsweise verwendet werden, um Daten aus einer CSV-Datei zu lesen oder um bestimmte Wörter oder Sätze aus einem längeren Text auszuwählen. In diesem Blog-Beitrag zeige ich Ihnen, wie Sie in TypeScript Teilsätze extrahieren können.

## Anleitung

Um Teilsätze aus einer Zeichenfolge zu extrahieren, können Sie die `substring()` Methode in TypeScript verwenden. Diese Methode nimmt zwei Parameter entgegen: den Startindex und den Endindex des Substrings. Hier ist ein Beispiel, das die Anwendung von `substring()` veranschaulicht:

```TypeScript
let text: string = "Dies ist ein Beispieltext.";
let substring: string = text.substring(5, 10);
console.log(substring); // gibt "ist e" aus
```

Wie Sie sehen können, gibt die `substring()` Methode die Zeichen zwischen dem Start- und Endindex (einschließlich des Startindex) zurück. Sie können auch negative Start- und Endindizes verwenden, um den Substring von hinten zu zählen.

## Tiefere Einblicke

Neben der `substring()` Methode bietet TypeScript auch die `substr()` Methode an, die ähnlich funktioniert, aber den optionalen Parameter `length` statt des Endindex verwendet. Wenn Sie den Endindex nicht angeben, gibt `substr()` alle Zeichen ab dem Startindex zurück. Hier ist ein Beispiel:

```TypeScript
let text: string = "Dies ist ein Beispieltext.";
let substring: string = text.substr(5, 10);
console.log(substring); // gibt "ist ein Bei" aus
```

Es gibt auch die `slice()` Methode, die ähnlich wie `substring()` funktioniert, aber negative Indizes verwendet, um von hinten zu zählen. Sie können die Unterschiede zwischen diesen drei Methoden erforschen und die für Ihre Anwendung am besten geeignete auswählen.

## Siehe auch
- [Dokumentation zu `substring()` in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Dokumentation zu `substr()` in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Dokumentation zu `slice()` in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)