---
title:                "Gleam: Html-Parsing"
simple_title:         "Html-Parsing"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

HTML ist eine der grundlegenden Technologien, die das moderne Web antreiben. Es ist die Sprache, die verwendet wird, um Webseiten zu erstellen und zu gestalten. Als Entwickler möchtest du möglicherweise die Inhalte von HTML-Webseiten extrahieren, um sie in deinen eigenen Anwendungen zu verwenden. Das Parsen von HTML ist der Prozess, bei dem der strukturierte Inhalt einer Webseite ausgelesen wird. In diesem Blogbeitrag werde ich dir zeigen, wie du dies mit der Programmiersprache Gleam erreichen kannst.

## Wie geht es das?

Um mit dem Parsen von HTML in Gleam zu beginnen, müssen wir zunächst ein Modul importieren, das uns beim Lesen und Verarbeiten von HTML helfen wird. Dieses Modul heißt "gleam/html". Wir verwenden die Funktion "parse", um eine HTML-Seite als Eingabe zu übergeben und erhalten als Ergebnis ein XML-Dokument. Dieses Dokument können wir dann analysieren und die Daten, nach denen wir suchen, extrahieren.

```Gleam
import gleam/html

html =
    """
    <html>
        <head>
            <title>Meine Website</title>
        </head>
        <body>
            <h1>Willkommen auf meiner Website</h1>
            <p>Hier findest du alle Informationen, die du brauchst.</p>
        </body>
    </html>
    """

doc = html/html.parse(html)

title =
    doc
    |> xml/child("head") // Unterelement "head" wählen
    |> xml/child("title") // Unterelement "title" wählen
    |> xml/text() // Text auswählen

welcome_message =
    doc
    |> xml/child("body")
    |> xml/child("h1")
    |> xml/text()

```

In diesem Beispiel haben wir eine einfache HTML-Seite definiert und sie an die "parse"-Funktion übergeben. Dann haben wir die XML-Dokument-Variable "doc" erstellt und zwei weitere Variablen "title" und "welcome_message" definiert, um den Titel und die Willkommensnachricht von der HTML-Seite zu extrahieren.

## Deep Dive

Beim Parsen von HTML in Gleam gibt es einige wichtige Konzepte zu beachten. Eine der wichtigsten ist die Verwendung von Pipes. Pipes ermöglichen es uns, eine Sequenz von Funktionen aneinander zu reihen, wobei die Ausgabe einer Funktion als Eingabe für die nächste verwendet wird. Dies kann sehr nützlich sein, um komplexe Datenstrukturen wie XML-Dokumente zu durchlaufen und die gewünschten Daten zu extrahieren.

Ein weiterer wichtiger Punkt ist die Verwendung von XPath-Ausdrücken. XPath ist eine Abfragesprache für XML-Dokumente, mit der wir bestimmte Elemente und deren Inhalte auswählen können. In unserem Beispiel haben wir die Funktion "child" verwendet, um ein bestimmtes Unterelement auszuwählen, und die Funktion "text", um den Text des Elements auszuwählen. Mit XPath können wir jedoch noch viel komplexere Abfragen durchführen, um die für uns relevanten Daten zu finden.

## Siehe auch

- [Offizielle Gleam Dokumentation](https://gleam.run/documentation/)
- [Dokumentation des "gleam/html"-Moduls](https://hexdocs.pm/gleam_html/readme.html)
- [Tutorial zu Gleam und HTML-Parsing](https://www.brian-kent.com/blog/2020/06/02/testing-gleam-html/)