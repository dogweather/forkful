---
title:                "Anführungszeichen aus einem String entfernen"
aliases:
- /de/javascript/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:37.704070-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Anführungszeichen aus einem String zu entfernen bedeutet, sich dieser lästigen Anführungszeichen zu entledigen, die Ihren Code durcheinanderbringen können, besonders wenn Sie Daten parsen oder JSON-Objekte konstruieren. Programmierer tun dies, um Eingaben zu bereinigen, Syntaxfehler zu vermeiden und Strings dazu zu bringen, sich gut mit anderen Teilen ihres Codes zu vertragen.

## Wie:
Stellen Sie sich vor, Sie haben einen String, der in Anführungszeichen eingeschlossen ist, wie `\"Hello, World!\"` und Sie möchten den reinen, unzitierten Text. Hier ist ein schnelles JavaScript-Snippet, um Ihren String aus den Fesseln der Zitate zu befreien:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Ausgabe: Hello, World!
```

Und wenn Sie es mit einfachen Anführungszeichen zu tun haben? Passen Sie einfach das Regex etwas an:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Ausgabe: Hello, World!
```

Oder was, wenn Ihr String eine Mischung aus beidem ist? Kein Problem:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Ausgabe: 'Hello, World!'
```

## Tiefergehend
Bevor JSON die Oberhand gewann, war das Entkommen von Anführungszeichen ein Wilder Westen aus Backslashes und Tricksereien. Frühe Programmiersprachen kamen nicht immer gut mit Anführungszeichen zurecht, was viel manuelle String-Manipulation nach sich zog. Jetzt, mit standardisierten Datenformaten, geht es beim Entfernen von Anführungszeichen oft darum, Eingaben zu säubern, bevor sie als JSON verarbeitet werden oder Text ohne Formatierungskonflikte gespeichert wird.

Alternativen zu `.replace()`? Sicher! Sie könnten einen String an den Anführungszeichen teilen und wieder zusammenfügen, `slice` verwenden, wenn Sie sich der Position Ihrer Anführungszeichen sicher sind, oder sogar Regex-Match verwenden, um den benötigten Text herauszuziehen. Es hängt alles vom Kontext ab.

Aber vergessen Sie nicht die Sonderfälle: Anführungszeichen in Anführungszeichen, escaped Anführungszeichen und internationale Zeichen. Denken Sie an Ihren String als ein mögliches Minenfeld von Ausnahmen und seien Sie vorsichtig. Moderne JavaScript-Engines sind optimiert, um Regex-Operationen effizient zu handhaben, daher sind sie im Allgemeinen der Weg, aber es lohnt sich immer, die Leistung für Aufgaben mit schwerer Datenverarbeitung zu überprüfen.

## Siehe auch
Tauchen Sie tiefer in die String-Manipulation und Regex ein:

- Mozilla Developer Network zu String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 zum Testen Ihrer Regex-Muster: https://regex101.com/
- JSON.org, um zu verstehen, warum wir uns in der modernen Webentwicklung mit so vielen Anführungszeichen beschäftigen: http://json.org/
