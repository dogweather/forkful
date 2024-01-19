---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess, bei dem der HTML-Code einer Webseite abgerufen wird. Programmierer machen das, um Informationen zu sammeln, Seiten zu testen oder Inhalte zu durchsuchen.

## So geht's:

In Gleam können Sie eine Webseite mittels des `gleam/httpc`-Moduls herunterladen. Sehen Sie sich das folgende Beispiel an.

```Gleam
import gleam/httpc
import gleam/http.{Request}

fn download() {
  let request = Request(
    method: httpc.Get, 
    url: "https://www.example.com", 
    headers: [], 
    body: httpc.Empty
  )
  let _ = httpc.send(request)
}
```
Die Ausgabe wäre der HTML-Text der Webseite.

```Gleam
{Ok, 
  Response(
    status: 200, 
    headers: [], 
    body: "..."
  )
}
```

## Tiefertauchen

Früher verwendeten Programmierer Shell-Dienstprogramme wie `wget` und `curl`, um Webseiten herunterzuladen. Heute bieten viele Sprachen eingebaute HTTP-Clients wie `httpc` in Gleam an. Alternativen in Gleam sind zum Beispiel die Verwendung einer HTTP-Bibliothek wie Gun oder Hackney.

Die Implementierung in Gleam basiert auf der OTP-Bibliothek, die auf vielen Ebenen der Netzwerkprogrammierung in Erlang verwendet wird. `httpc` ist ein einfacher HTTP-Client, der eine Asynchronität und Schutz vor Fehlern bietet.

## Siehe auch

- [Die Gleam HTTP Dokumentation](https://gleam.run/book/tour/http.html)
- [Gun HTTP Client Bibliothek](https://github.com/ninenines/gun)
- [hackney: Simple HTTP client](https://github.com/benoitc/hackney)