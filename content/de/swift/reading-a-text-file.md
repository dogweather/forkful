---
title:    "Swift: Einen Textdatei lesen."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist ein grundlegender Bestandteil der Programmierung. Es ermöglicht uns, Informationen von externen Quellen in unsere Anwendungen zu integrieren und zu verarbeiten. In diesem Blogbeitrag werden wir uns ansehen, wie wir Textdateien in Swift lesen können.

## Wie

Um eine Textdatei in Swift zu lesen, verwenden wir die ```String```-Klasse und die ```String(contentsOfFile:encoding:)``` -Methode. Der ```encoding```-Parameter gibt den Zeichensatz an, der beim Lesen der Datei verwendet werden soll. Wenn die Datei UTF-8 codiert ist, können wir einfach ```String(contentsOfFile: filename, encoding: .utf8)``` eingeben, um die Datei zu lesen.

Wenn wir beispielsweise eine Datei mit dem Namen "meinText.txt" haben, die den folgenden Inhalt enthält:

```
Hallo, Welt! 
Dies ist ein Test.
```

dann können wir folgenden Code verwenden, um die Datei zu lesen und den Inhalt in der Konsole auszugeben:

```
if let text = String(contentsOfFile: "meinText.txt", encoding: .utf8) {
    print(text)
}
```

Dies wird "Hallo, Welt! Dies ist ein Test." in der Konsole ausgeben.

## Deep Dive

Es ist wichtig zu beachten, dass die ```String(contentsOfFile:encoding:)``` -Methode eine werfen kann ```String``` , die möglicherweise ```nil``` falls die Datei nicht gelesen werden kann. Um dies zu vermeiden, können wir das optionale Bindungsmuster verwenden, um sicherzustellen, dass wir ein gültiges ```String```-Objekt erhalten. Wir können auch den ```path``` -Parameter verwenden, um den absoluten Pfad zur Datei anzugeben, anstatt nur den Dateinamen zu verwenden.

Es gibt auch weitere Möglichkeiten, Textdateien in Swift zu lesen, wie z.B. die Verwendung von Streams oder die Verwendung von ```Data``` -Objekten, um die Roh-Daten aus der Datei zu lesen. Für weitere Informationen empfehlen wir die offizielle Swift-Dokumentation zu diesem Thema.

## Siehe auch

- [Offizielle Swift-Dokumentation zu Dateioperationen](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID300)
- [Einen Text aus einer Datei lesen in Objective-C](https://www.techotopia.com/index.php/Working_with_Files_in_Objective-C)
- [Ein Update auf Swift: Einen Text aus einer Datei lesen](https://stackoverflow.com/questions/31169654/an-update-to-swift-reading-a-text-file)