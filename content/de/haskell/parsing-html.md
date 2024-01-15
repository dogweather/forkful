---
title:                "HTML-Analyse"
html_title:           "Haskell: HTML-Analyse"
simple_title:         "HTML-Analyse"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand sich mit dem Parsen von HTML beschäftigen? Nun, HTML ist die Sprache, die verwendet wird, um Webseiten zu erstellen und es gibt Millionen von Webseiten da draußen. Es ist also sehr nützlich, HTML zu verstehen, um Webinhalte zu extrahieren oder zu manipulieren.

## Wie geht das?

Um mit dem Parsen von HTML in Haskell zu beginnen, müssen wir zunächst das Paket "html-conduit" mit dem Paketmanager stack installieren. Anschließend können wir mit dem Importieren des Moduls "Text.HTML.DOM" und der Funktion "parseLBS" starten.

```Haskell
import Text.HTML.DOM
import Data.Text
import Data.ByteString.Lazy
import Text.XML
```

Wir können nun eine HTML-Datei einlesen und sie in eine Datenstruktur transformieren, die wir in unserem Code verwenden können.

```Haskell
getFileContents :: IO ByteString
getFileContents = readFile "page.html"

contents <- getFileContents
let doc = parseLBS contents

print doc
```

Die Ausgabe wird so aussehen:

```Haskell
<div>
    <h1>Meine Webseite</h1>
    <p>Willkommen auf meiner Webseite!</p>
    <ul>
        <li><a href="https://www.example.com">Beispiel-Link 1</a></li>
        <li><a href="https://www.example2.com">Beispiel-Link 2</a></li>
        <li><a href="https://www.example3.com">Beispiel-Link 3</a></li>
    </ul>
</div>
```

Wir können nun bestimmte Elemente in dieser Datenstruktur auswählen und sie in unsere Ausgabe einfügen. Zum Beispiel können wir die ersten beiden Links aus der Liste extrahieren und anzeigen lassen.

```Haskell
let links = doc $// el "a" &/ content
putStrLn $ "Die ersten beiden Links sind: " ++ links !! 0 ++ " und " ++ links !! 1
```

Die Ausgabe wird so aussehen:

```Haskell
Die ersten beiden Links sind: Beispiel-Link 1 und Beispiel-Link 2
```

## Tiefer Einblick

Beim Parsen von HTML ist es wichtig zu beachten, dass nicht alle HTML-Dokumente korrekt formatiert sind. Es gibt verschiedene Tools und Bibliotheken, die dabei helfen, Fehler in HTML-Dateien zu erkennen und zu beheben. Außerdem kann es nützlich sein, sich mit den verschiedenen DOM- und XPath-Methoden vertraut zu machen, um die Auswahl von bestimmten Elementen zu erleichtern.

## Siehe auch

- "A Guide to HTML parsing in Haskell" von Michael Snoyman: https://www.yesodweb.com/blog/2010/05/html-parsing
- "A Beginner's Guide to HTML Parsing in Haskell with tagsoup" von Taymon Beal: https://taymonbeal.medium.com/a-beginners-guide-to-html-parsing-in-haskell-with-tagsoup-f5d70f506112
- "html-conduit Dokumentation" auf Hackage: https://hackage.haskell.org/package/html-conduit