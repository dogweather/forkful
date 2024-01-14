---
title:                "Swift: Eine Textdatei lesen."
simple_title:         "Eine Textdatei lesen."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien spielen in der Programmierung eine wichtige Rolle, da sie eine einfache und universelle Möglichkeit bieten, Daten zu speichern und zu verarbeiten. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie wir mithilfe von Swift Programmierung eine Textdatei einlesen und verarbeiten können.

## Wie geht das?

Um eine Textdatei in Swift zu lesen, nutzen wir die Funktion `String(contentsOfFile:encoding)`, welche den Inhalt der Datei als String zurückgibt. Wir möchten dabei beachten, dass wir den Pfad zur Datei sowie die richtige Zeichenkodierung angeben müssen. Als Beispiel nutzen wir eine einfache Textdatei mit dem Inhalt "Hello World!".

``` Swift
let path = "pfad/zur/datei.txt" 
let encoding = String.Encoding.utf8 
if let content = try? String(contentsOfFile: path, encoding: encoding) { 
  print(content) 
}
```

Die Ausgabe dieses Codeschnipsels wird "Hello World!" sein. Wir haben erfolgreich den Inhalt der Textdatei in die `content` Variable geladen und diesen anschließend ausgegeben.

## Tiefer Einblick

Beim Einlesen von Textdateien in Swift gibt es ein paar Dinge zu beachten. Zum einen sollte der Pfad zur Datei absolut und nicht relativ sein, um sicherzustellen, dass die Datei immer an der gleichen Stelle gefunden wird. Zudem ist es wichtig, die korrekte Zeichenkodierung anzugeben, um sicherzustellen, dass Sonderzeichen und Umlaute korrekt dargestellt werden.

Des Weiteren gibt es verschiedene Methoden, um den Inhalt einer Textdatei weiterzuverarbeiten, wie zum Beispiel das Herausfiltern von bestimmten Informationen oder das Speichern des Inhalts in einer Datenstruktur. Auch das Einlesen mehrerer Textdateien ist möglich, indem man eine Schleife nutzt und den Pfad zur Datei dynamisch ändert.

## Siehe auch

- [Ressource zu Swift Dateiverarbeitung](https://www.raywenderlich.com/9265-filemanager-class-tutorial-for-macos-how-to-read-and-write-files)
- [Dokumentation von Swift String](https://developer.apple.com/documentation/swift/string)