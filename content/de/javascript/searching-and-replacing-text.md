---
title:                "Javascript: Textsuche und -ersetzung"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Suchen und Ersetzen von Text ist ein wichtiger Teil der Programmierung. Es ermöglicht uns, schnell und effizient Fehler zu beheben, bestimmte Teile des Codes zu ändern oder Daten zu aktualisieren. Mit JavaScript können wir diese Funktionen einfach in unsere Programme integrieren und somit eine bessere Kontrolle über unseren Code haben.

## Wie funktioniert es

Es gibt mehrere Möglichkeiten, um Text in JavaScript zu suchen und zu ersetzen. Wir nutzen dazu die Funktion `replace()`, die in der String-Klasse eingebaut ist. Das folgende Beispiel zeigt, wie wir alle Vorkommnisse eines bestimmten Textes in einem String ersetzen können:

```
let text = "Dies ist ein Beispieltext, der einige Fehler enthält.";
let neuerText = text.replace("Fehler", "Änderungen");

console.log(neuerText); // Ausgabe: "Dies ist ein Beispieltext, der einige Änderungen enthält."
```

Wir können auch eine reguläre Ausdrucksübereinstimmung verwenden, um gezieltere Suchen und Ersetzungen durchzuführen. Zum Beispiel können wir alle Zahlen in einem String durch Nullen ersetzen, indem wir folgenden Code verwenden:

```
let text = "Ich habe 3 Äpfel, 2 Birnen und 5 Orangen.";
let neuerText = text.replace(/[0-9]/g, "0");

console.log(neuerText); // Ausgabe: "Ich habe 0 Äpfel, 0 Birnen und 0 Orangen."
```

Es gibt auch die Möglichkeit, eine Funktion als zweites Argument anzugeben, die den Übereinstimmungen verschiedene Aktionen zuweisen kann. In diesem Beispiel geben wir allen Vokalen in einem String den entsprechenden Vokal aus der zweiten Hälfte des Alphabets aus:

```
let text = "JavaScript";
let neuerText = text.replace(/[aeiou]/g, (vokal) => String.fromCharCode(vokal.charCodeAt(0) + 5));

console.log(neuerText); // Ausgabe: "PhfwwdVyf%"
```

## Tieferes Eintauchen

Um effizienter mit der `replace()`-Funktion umgehen zu können, können wir auch einige zusätzliche Parameter verwenden. Zum Beispiel können wir `i` hinzufügen, um die Suche nach Texten unabhängig von Groß- und Kleinschreibung zu machen, oder `g` für eine globale Suche, die alle Übereinstimmungen im String ersetzt. Wir können auch Callback-Funktionen verwenden, um die Übereinstimmungen individuell zu bearbeiten. Eine detaillierte Anleitung finden Sie in der offiziellen [Dokumentation von JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace).

## Siehe auch

- [String-Klasse von JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Reguläre Ausdrücke in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Grundlegendes zu JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript)