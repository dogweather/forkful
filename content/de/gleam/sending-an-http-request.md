---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:59:28.249823-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind Mechanismen, mit denen deine Programme im Netz Daten anfordern oder senden. Programmierer nutzen sie, um Web-APIs anzusprechen, Inhalte auszutauschen oder Dienste zu konsumieren.

## How to:
```gleam
import gleam/http
import gleam/httpc

pub fn fetch_data() {
  // Definiere deine URL
  let url = "https://api.example.com/data"
  
  // Erstelle eine GET-Anfrage
  case httpc.send(http.Request(method: Get, url: url)) {
    Ok(response) -> 
      // Zeige den Erfolg an, wenn alles passt
      io.println("Daten empfangen: ")
      io.println(response.body)

    Error(error) ->
      // Fehlerbehandlung, falls etwas schief geht
      io.println("Fehler bei der Anfrage: ")
      io.println(error)
  }
}
```
Ausgabe:
```text
Daten empfangen: {Json data here...}
```

oder, im Fehlerfall:

```text
Fehler bei der Anfrage: {Error details...}
```

## Deep Dive
HTTP wurde Anfang der 90er entwickelt und ist das Fundament des Datenverkehrs im Web. Alternativen zu `httpc` in Gleam sind Libraries wie `opium`, die mehr Funktionen bieten könnten. Die Gleam-Implementierung achtet auf Typensicherheit und Kompaktheit, so dass Fehler möglichst schon zur Compilezeit ausgeschlossen werden können.

## See Also
- [Getting started with Gleam](https://gleam.run/book/)