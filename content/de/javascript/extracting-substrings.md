---
title:                "Javascript: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Du hast gerade deine ersten Schritte in der Programmierwelt gemacht und fragst dich jetzt vielleicht: Warum sollte ich überhaupt Substrings extrahieren? Nun, Substrings sind Teilstrings innerhalb eines größeren Strings und können nützlich sein, um bestimmte Daten aus Texten zu extrahieren oder zu manipulieren. Zum Beispiel könntest du einen Nutzernamen aus einer E-Mail-Adresse extrahieren oder einen Teil eines Textes durch einen anderen ersetzen. Das Extrahieren von Substrings ist eine wichtige Fähigkeit, die dir bei vielen Aufgaben in der Programmierung helfen kann.

## Wie geht's

Um Substrings in Javascript zu extrahieren, gibt es verschiedene Methoden. Eine davon ist die `substring()`-Methode, die einen Teil des ursprünglichen Strings basierend auf dem Start- und Endindex zurückgibt. Zum Beispiel:

```Javascript
let str = "Hallo Welt";
let sub = str.substring(0, 5); // sub enthält jetzt "Hallo"
```

Hier haben wir den Substring von Index 0 bis Index 5 aus dem String extrahiert. Beachte, dass der Endindex nicht mit extrahiert wird, deshalb ist der Teilstrings "Hallo" und nicht "Hallo W".

Eine andere Möglichkeit ist die `slice()`-Methode, bei der du einen Startindex und einen optionalen Endindex angeben kannst. Wenn kein Endindex angegeben wird, wird der Teilstring bis zum Ende des ursprünglichen Strings extrahiert. Beispiel:

```Javascript
let str = "Hallo Welt";
let sub = str.slice(6); // sub enthält jetzt "Welt"
```

Diese Methode ist besonders nützlich, wenn du Substrings aus einer bestimmten Position bis zum Ende eines Strings extrahieren möchtest.

## Tiefen Einblick

Neben den `substring()`- und `slice()`-Methoden gibt es noch weitere Möglichkeiten, Substrings in Javascript zu extrahieren. Die `substr()`-Methode zum Beispiel, die ähnlich wie `slice()` funktioniert, aber der zweite Parameter gibt die Länge des Substrings anstatt des Endindex an.

```Javascript
let str = "Hallo Welt";
let sub = str.substr(6, 4); // sub enthält jetzt "Welt"
```

Eine andere Methode, die zur Extraktion von Substrings verwendet werden kann, ist die `match()`-Methode, die mithilfe von Regulären Ausdrücken den gewünschten Teil eines Strings zurückgibt.

```Javascript
let str = "Hallo Welt";
let sub = str.match(/Welt/); // sub enthält jetzt "Welt"
```

Es gibt noch weitere Methoden, die sich je nach Anforderung und Situation besser eignen können. Es ist empfehlenswert, sich mit allen verfügbaren Methoden auseinanderzusetzen, um die beste Lösung für dein Anliegen zu finden.

## Siehe auch

- [MDN Web Docs - Substring](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - Slice](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs - Substr](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs - Match](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/match)