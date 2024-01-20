---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML bezieht sich auf den Prozess, in dem ein HTML-Dokument analysiert und seine Struktur erfasst wird. Programmierer machen dies, um auf bestimmte Daten zuzugreifen oder Webseiten zu scrapen.

## Wie zu:

In Gleam könnte der Code zum Parsen eines HTML-Dokuments mit der Bibliothek `gleam/http` folgendermaßen aussehen.

```Gleam
import gleam/http.{get, start_link}

fn main(_) {
  case start_link() {
    Error(e) -> io.println(e)
    Ok(_) ->
      case get("https://example.com") {
        Error(e) -> io.println(e)
        Ok(response) -> io.println(response.body)
      }
  }
}
```
Führen Sie diesen Code aus und Sie werden den HTML-Inhalt der Webseite `https://example.com` auf der Konsole anzeigen.

## Deep Dive

Das Parsen von HTML hat eine lange Geschichte, die bis in die frühen Tage des Webs zurückreicht, als Entwickler anfingen, Daten von Websites zu extrahieren. Alternativen zum Parsen von HTML sind das Screen-Scraping oder der Datenzugriff über APIs, sofern diese vorhanden sind.

In Gleam können Sie bei der Implementierung von HTML-Parsing auf bestehende Bibliotheken wie Beautiful Soup oder Html Agility Pack zurückgreifen, wenn Sie die Interoperabilität mit Sprachen wie Python oder .NET nutzen möchten.

## Siehe Auch

Für weitere Informationen zu Gleam und der Verwendung von HTTP-Anfragen in Gleam, besuchen Sie bitte die offizielle Gleam-Dokumentation (https://gleam.run/docs/introduction/). Sie können auch das Gleam-Repository auf GitHub besuchen (https://github.com/gleam-lang/http) für detaillierte Beispiele und Diskussionen zur Verwendung der `gleam/http` Bibliothek.