---
title:    "Swift: Verwendung von regulären Ausdrücken"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum Regular Expressions in Swift verwenden?

Reguläre Ausdrücke (engl. Regular Expressions) sind eine leistungsstarke Möglichkeit, Texte zu durchsuchen und zu bearbeiten. Sie ermöglichen es, komplexe Muster in Strings zu erkennen und zu manipulieren. In der Swift-Programmierung können reguläre Ausdrücke in vielen Fällen eine effiziente Lösung für Textmanipulation sein. In diesem Artikel werden wir uns genauer ansehen, wie man reguläre Ausdrücke in Swift nutzen kann.

## Wie man reguläre Ausdrücke in Swift verwendet

Die Grundidee hinter regulären Ausdrücken ist einfach: Sie definieren ein Muster, nach dem ein String durchsucht werden soll. Dazu können spezielle Zeichen wie Klammern, Sternchen oder Pluszeichen genutzt werden, die bestimmte Zeichen oder Muster repräsentieren. In Swift können reguläre Ausdrücke mithilfe der Klasse "NSRegularExpression" verwendet werden. Hier ein Beispiel, wie man einen String nach einer bestimmten Zeichenfolge durchsuchen kann:

```Swift
let string = "Ich liebe Swift!"
let regex = try? NSRegularExpression(pattern: "Swift")
let matches = regex?.matches(in: string, range: NSRange(location: 0, length: string.count))
for match in matches! {
    print(match.range) // output: (range: 9, 5)
}
```

Wir definieren hier zunächst einen String und erstellen dann ein Objekt der Klasse "NSRegularExpression", welches unser zu durchsuchendes Muster beinhaltet. Mit der Methode "matches(in:range)" können wir dann alle Übereinstimmungen in unserem String finden und diese ausgeben. In diesem Fall wird die Range des Strings "Swift" zurückgegeben.

## Tiefere Einblicke in die Verwendung von regulären Ausdrücken

Reguläre Ausdrücke bieten viele Möglichkeiten, um komplexe Muster in Texten zu finden und zu manipulieren. Dazu gibt es auch verschiedene Optionen, die wir bei der Definition eines Musters festlegen können. Zum Beispiel können wir angeben, ob Groß- und Kleinschreibung berücksichtigt werden sollen, oder ob wir nur nach dem ersten oder allen Vorkommen suchen möchten. Auch das Ersetzen von Texten mit regulären Ausdrücken ist möglich. Hier ein Beispiel, wie man ein Wort in einem String ersetzen kann:

```Swift
let string = "Das ist ein Text."
let regex = try? NSRegularExpression(pattern: "Text")
let newString = regex?.stringByReplacingMatches(in: string, options: [], range: NSRange(location: 0, length: string.count), withTemplate: "Text-Muster")
print(newString) // output: "Das ist ein Text-Muster."
```

Die Methode "stringByReplacingMatches(in:options:range:withTemplate:)" ermöglicht es uns, alle Vorkommen des Musters zu ersetzen. Hier haben wir angegeben, dass wir die Option "ignoreCase" nutzen möchten, um auch nach unteschiedlichen Groß- und Kleinschreibungen zu suchen.

# Siehe auch

- [NSRegularExpression-Klasse in der Apple Dokumentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Reguläre Ausdrücke - Erklärt auf Swift Blog](https://www.swiftblog.de/2018/02/13/regular-expressions-swift-erklärt/)
- [Regex101 - Online Tool zum Testen von regulären Ausdrücken](https://regex101.com/)