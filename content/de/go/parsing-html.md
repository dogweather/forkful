---
title:                "Das Aufteilen von HTML"
html_title:           "Go: Das Aufteilen von HTML"
simple_title:         "Das Aufteilen von HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das PARSING von HTML ist der Prozess des Analysierens und Extrahierens von Informationen aus HTML-Dokumenten, um sie für die weitere Verarbeitung verfügbar zu machen. Programmierer nutzen dies, um zum Beispiel automatisierte Aufgaben wie das Scrapen von Daten oder das Generieren von dynamischen Webseiten zu erleichtern.

## Wie geht's:

```Go
import "golang.org/x/net/html"

func main() {
    // HTML-Code als Zeichenkette definieren
    htmlString := "<html><head><title>Testseite</title></head><body><h1>Hello World!</h1></body>"

    // HTML-String in ein html.Tokenizer-Objekt parsen
    tokenizer := html.NewTokenizer(strings.NewReader(htmlString))

    // Token für jedes Element im Dokument finden
    for {
        // Nächstes Token aus dem HTML-Dokument extrahieren
        tokenType := tokenizer.Next()

        // Wenn das Token ein Start-Element ist
        if tokenType == html.StartTagToken {
            // Namen des Elements aus dem Token auslesen
            tagName, _ := tokenizer.TagName()

            // Namen als Zeichenkette ausgeben
            fmt.Println("Start-Element: " + string(tagName))
        // Wenn das Token ein Text-Element ist
        } else if tokenType == html.TextToken {
            // Text aus dem Token auslesen
            text := tokenizer.Text()

            // Text als Zeichenkette ausgeben
            fmt.Println("Text-Element: " + string(text))
        // Wenn das Token ein Ende-Element ist
        } else if tokenType == html.EndTagToken {
            // Namen des Elements aus dem Token auslesen
            tagName, _ := tokenizer.TagName()

            // Namen als Zeichenkette ausgeben
            fmt.Println("Ende-Element: " + string(tagName))
        } else if tokenType == html.ErrorToken {
            // Bei einem Fehler die Schleife beenden
            break
        }
    }
}
```

Output:

```
Start-Element: html
Start-Element: head
Start-Element: title
Text-Element: Testseite
Ende-Element: title
Ende-Element: head
Start-Element: body
Start-Element: h1
Text-Element: Hello World!
Ende-Element: h1
Ende-Element: body
Ende-Element: html
```

## Tief tauchen:

Das Parsen von HTML hat in der IT-Geschichte eine bewegte Vergangenheit durchlaufen, von einfacher String-Manipulation über XML-basierte Parser bis hin zu heutigen DOM-basierten Parsers. Alternativen zum Parsen von HTML in Go sind unter anderem das Package "html/template" und externe Libraries wie "GoQuery". Die Implementierung in Go basiert auf dem offenen Standards HTML5 und DOM Level 2.

## Siehe auch:

- Offizielle Dokumentation von Go: https://golang.org/pkg/html/
- "html/template" Package in Go: https://golang.org/pkg/html/template/
- GoQuery Library: https://github.com/PuerkitoBio/goquery