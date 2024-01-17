---
title:                "Eine Zeichenkette interpolieren"
html_title:           "TypeScript: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Einfügen von Variablen oder Ausdrücken in einen String wird als String-Interpolation bezeichnet. Programmierer verwenden es, um dynamischen Inhalt in einen Text zu integrieren und die Lesbarkeit von Code zu verbessern.

## Wie?

Codebeispiele mit Ausgaben innerhalb von ```TypeScript ... ``` Codeblöcken:

```
// Beispiel 1:
const name = "Maria";
console.log(`Hallo ${name}, wie geht es dir?`);
// Ausgabe: Hallo Maria, wie geht es dir?

// Beispiel 2:
const x = 5;
const y = 10;
console.log(`Die Summe von ${x} und ${y} ist ${x + y}.`);
// Ausgabe: Die Summe von 5 und 10 ist 15.
```

## Tiefer Einblick

- Historischer Kontext: String-Interpolation wurde erstmals in der Programmiersprache Perl eingeführt und ist seitdem in vielen anderen Sprachen wie Python, Ruby und TypeScript verfügbar.

- Alternativen: Eine andere Möglichkeit, dynamischen Inhalt in einen String einzufügen, ist die Verwendung von String-Konkatenation. Dies ist jedoch umständlicher und kann zu unleserlichem Code führen.

- Implementierungsdetails: In TypeScript wird String-Interpolation durch Verwendung der Backtick-Notation ``` ` ` `` ermöglicht. Der Inhalt innerhalb der geschweiften Klammern wird als Ausdruck ausgewertet und in den String eingefügt.

## Siehe auch

Weitere Informationen und Beispiele zur String-Interpolation in TypeScript finden Sie in der offiziellen Dokumentation: https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-interpolation.