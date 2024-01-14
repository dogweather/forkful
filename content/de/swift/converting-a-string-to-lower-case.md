---
title:    "Swift: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren einer Zeichenkette in Kleinbuchstaben kann in Swift nützlich sein, um Konsistenz in der Textdarstellung zu gewährleisten oder um die Benutzereingaben zu standardisieren.

# Wie geht es

````Swift
//Input
let string = "Hello World!"

//Verwendung der lowercased()-Methode
let lowercasedString = string.lowercased()

//Output
print(lowercasedString) //hallo welt!
````
Um eine Zeichenkette in Kleinbuchstaben zu konvertieren, können Sie die vorinstallierte lowercased()-Methode verwenden. Diese Methode gibt eine neue Zeichenkette mit allen Buchstaben in Kleinbuchstaben zurück. 

Alternativ können Sie auch die Funktion localizedLowercase() verwenden, um eine Zeichenkette basierend auf der aktuellen Spracheinstellung des Geräts in Kleinbuchstaben zu konvertieren.

````Swift
//Input
let string = "Bonjour"

//Verwendung der localizedLowercase()-Funktion
let lowercasedString = string.localizedLowercase()

//Output
print(lowercasedString) //bonjour
````

# Tiefer gehende Erklärung

Beim Konvertieren einer Zeichenkette in Kleinbuchstaben muss nicht nur an die englische Sprache und das lateinische Alphabet gedacht werden. Zum Beispiel haben einige Sprachen wie das Deutsche, Türkische und Ungarische spezielle Buchstaben wie Ä, Ö, Ü und Sz, die auch in Kleinbuchstaben umgewandelt werden müssen. Hier kommt die Funktion localizedLowercase() zum Einsatz, die solche Besonderheiten berücksichtigt.

Zusätzlich ist zu beachten, dass die Konvertierung von Groß- und Kleinbuchstaben in manchen Sprachen nicht "eindeutig" ist. Zum Beispiel gibt es im Deutsch keinen eindeutigen Groß- oder Kleinbuchstaben für den Buchstaben "ß". Hier muss sich der Entwickler bewusst sein, welche Methode oder Funktion für die Konvertierung geeignet ist und zu welchem Zweck sie verwendet werden soll.

# Siehe auch

- [Die offizielle Swift-Dokumentation zu String-Manipulation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stack Overflow - Wie kann ich eine Zeichenkette in Kleinbuchstaben konvertieren?](https://stackoverflow.com/questions/29076657/how-to-convert-string-to-lowercase-in-swift)
- [Swift Blog - Localization in Swift: Eine detaillierte Anleitung](https://developer.apple.com/swift/blog/?id=25)