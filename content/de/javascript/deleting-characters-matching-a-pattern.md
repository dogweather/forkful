---
title:                "Javascript: Musterorientiertes Löschen von Zeichen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie regelmäßig mit Strings in JavaScript arbeiten, kennen Sie vielleicht das Problem: Sie haben einen String, aber es gibt einige Zeichen, die Sie nicht darin haben möchten. Vielleicht sind es Leerzeichen oder Sonderzeichen, die Sie aus dem String entfernen müssen. In solchen Fällen kann es hilfreich sein, Zeichen zu löschen, die einem bestimmten Muster entsprechen. In diesem Beitrag lernen Sie, wie Sie das in JavaScript tun können.

# Wie man Zeichen löscht, die einem Muster entsprechen

```javascript
// Ein Beispielstring
var string = "H€ll0 w0rld";

// Löscht alle Leerzeichen
var modifiedString = string.replace(/\s/g, "");

// Löscht alle Zahlen
modifiedString = string.replace(/\d/g, "");

// Löscht alle Sonderzeichen
modifiedString = string.replace(/[^\w\s]/gi, "");

console.log(modifiedString);
// Output: Helloworld
```
In diesem Beispiel werden verschiedene reguläre Ausdrücke verwendet, um Leerzeichen, Zahlen und Sonderzeichen aus dem String zu entfernen. Hier sind einige nützliche Ausdrücke, die Sie ausprobieren können:

- `\s` steht für alle Leerzeichen
- `\d` steht für alle Zahlen
- `\w` steht für alle alphanumerischen Zeichen (Buchstaben und Zahlen)
- `[^\w\s]` steht für alle Zeichen, die weder alphanumerisch noch Leerzeichen sind

# Tiefere Einblicke

Die replace() Methode in JavaScript verwendet reguläre Ausdrücke, um bestimmte Zeichen in einem String zu finden und zu ersetzen. Sie können nicht nur einzelne Zeichen, sondern auch Muster von Zeichen in einem String löschen. In unserem Beispiel haben wir die globale Flagge (`g`) verwendet, um alle Vorkommen eines Musters im String zu ersetzen. Sie können auch die Fallunterscheidung (`i`) Flagge verwenden, um zwischen Groß- und Kleinschreibung zu unterscheiden.

Sie können auch die replace() Methode verwenden, um Zeichen durch andere Zeichen zu ersetzen, anstatt sie zu löschen. Wenn Sie beispielsweise alle Vorkommen von "e" in einem String durch "a" ersetzen möchten, können Sie folgenden Code verwenden:

```javascript
var string = "H€ll0 w0rld";
var modifiedString = string.replace(/e/g, "a");
console.log(modifiedString);
// Output: Hall0 w0rld
```

Dies ist nur ein einfaches Beispiel für die Verwendung der replace() Methode in JavaScript. Sie können noch viel mehr tun, indem Sie mit regulären Ausdrücken experimentieren. Für weitere Informationen empfehle ich, die offizielle Dokumentation von JavaScript zu lesen.

# Siehe auch

- [String replace() Methode in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Reguläre Ausdrücke in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr - Online RegEx Test Tool](https://regexr.com/)