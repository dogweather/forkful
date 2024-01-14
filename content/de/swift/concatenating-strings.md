---
title:    "Swift: Verkettung von Zeichenfolgen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen oder Verketten von Strings ist eine häufig verwendete Technik in der mobilen App-Entwicklung. Es ermöglicht uns, variablen Inhalt in eine einzige Zeichenfolge zu kombinieren und so Texte dynamisch zu erstellen. Dies kann nützlich sein, um beispielsweise personalisierte Benachrichtigungen an Benutzer zu senden oder dynamische Textinhalte auf einer Benutzeroberfläche anzuzeigen.

## How To

Es gibt verschiedene Möglichkeiten, Strings in Swift zu verketten, aber die gebräuchlichste ist die Verwendung des "+=" Operators. Hier ist ein Beispiel:

```Swift
var name = "Paula"
var greeting = "Hallo "

greeting += name
print(greeting)
```
Output: "Hallo Paula"

Es ist auch möglich, Strings mithilfe des `"\()"`-Operators in eine Zeichenfolge einzubetten. Hier ist ein Beispiel:

```Swift
var age = 32
var message = "Ich bin \(age) Jahre alt."
print(message)
```
Output: "Ich bin 32 Jahre alt."

## Deep Dive

Swift bietet auch die `joined()` Methode, um eine Sequenz von Strings in eine einzelne Zeichenfolge zu verketten. Hier ist ein Beispiel:

```Swift
var fruits = ["Apfel", "Banane", "Orange"]
var fruitsList = fruits.joined(separator: ", ")
print(fruitsList)
```
Output: "Apfel, Banane, Orange"

Auch der `split()` Operator kann verwendet werden, um eine Zeichenfolge in mehrere Teile zu zerlegen, die dann wieder zusammengesetzt werden können. Hier ist ein Beispiel:

```Swift
var words = "Hallo, mein Name ist Max"
var wordArray = words.split(separator: " ")
var hello = String(wordArray[0])
var name = String(wordArray[3])
var message = "\(hello), ich heiße \(name)."
print(message)
```
Output: "Hallo, ich heiße Max."

Es ist wichtig zu beachten, dass das Concatenieren von Strings immer ein neues Objekt erstellt und somit zu einer schlechteren Leistung führen kann, wenn es in einer Schleife verwendet wird. In diesem Fall ist es möglicherweise besser, einen `StringBuffer` zu verwenden, da dieser das hinzufügen von Strings ohne die Erstellung von Zwischenspeicher ermöglicht.

# Siehe auch

- [Swift Strings Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [StringAPI Reference](https://developer.apple.com/documentation/swift/string)