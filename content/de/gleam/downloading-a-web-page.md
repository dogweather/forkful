---
title:                "Herunterladen einer Webseite"
html_title:           "Gleam: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn wir eine Webseite besuchen, wird sie auf unseren Bildschirm geladen und wir können sie lesen und nutzen. Aber was passiert eigentlich im Hintergrund, wenn wir eine Webseite "runterladen"? In der Programmierwelt wird dieser Vorgang als "Webseiten-Download" bezeichnet und ist ein wichtiger Teil der Entwicklung von Web-Anwendungen.

## Wie geht das?

Das Herunterladen einer Webseite kann in Gleam mit Hilfe des `httpc`-Moduls einfach umgesetzt werden. Wir benutzen die Funktion `httpc.get`, um eine URL anzugeben und einen `on_success`-Callback, der aufgerufen wird, wenn die Seite erfolgreich heruntergeladen wurde.

```Gleam
httpc.get("https://www.example.com", on_success: \response -> {
    // `response` ist der heruntergeladene Inhalt
    switch response {
        Ok(body) -> {
            // Der Inhalt wird in der Reihenfolge angezeigt, in der er angegeben wurde
            Console.print(body)
            // => "<!DOCTYPE html>...
            //    <title>Welcome to example.com</title>..."
        }
        Err(error) -> {
            Console.print("Oops! Etwas ist schief gelaufen!")
        }
    }
})
```

## Tiefere Einblicke

Das Herunterladen von Webseiten ist ein wichtiger Teil der Webentwicklung, da viele Web-Anwendungen auf die Daten von externen Quellen angewiesen sind. Beispielsweise können wir mit dem Inhalt einer heruntergeladenen Webseite Informationen für unsere Anwendung abrufen oder überprüfen, ob eine bestimmte Seite verfügbar ist.

Eine Alternative zum `httpc`-Modul ist das beliebte `curl`-Tool, das jedoch nicht so einfach zu bedienen ist und mehr Einarbeitungszeit erfordert. Mit Hilfe von `httpc` können wir jedoch schnell und effizient Webseiten herunterladen und in unsere Anwendungen integrieren.

In der Tiefe der Implementierung verwendet das `httpc`-Modul das HTTP-Protokoll, um Anfragen an eine Webseite zu senden und deren Antwort zu empfangen. Dies erfordert Kenntnisse über Netzwerkprogrammierung und Protokolle, aber dank Gleam können wir dies einfach und sicher umsetzen.

## Siehe auch

- Das offizielle Gleam `httpc`-Modul: https://github.com/gleam-lang/httpc
- Ein umfassendes Tutorial zu Webentwicklung mit Gleam: https://github.com/gleam-lang/gleam_web_tutorial