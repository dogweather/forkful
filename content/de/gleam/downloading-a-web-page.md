---
title:                "Webseite herunterladen"
date:                  2024-01-20T17:43:51.728306-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Webseiten herunterladen bedeutet, den Inhalt einer Webseite auf deinen Rechner zu übertragen. Programmierer machen das, um Daten zu sammeln, Services zu automatisieren oder Inhalte offline verfügbar zu machen.

## How to:
Gleam verwendet die `http` Bibliothek für Webanfragen. Sieh hier wie's geht:

```gleam
import gleam/http
import gleam/httpc

pub fn main() {
  let result = httpc.send(req())
  case result {
    Ok(response) -> response.body
    Error(error) -> "An error occurred"
  }
}

fn req() -> http.Request {
  http.Request(
    method: http.Get,
    url: "http://example.com",
    headers: [],
    body: http.Nil,
  )
}
```

Ausgabe:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive
Das Herunterladen von Webseiten ist so alt wie das Web selbst. Früher verwendete man Werkzeuge wie `wget` oder `curl`. Heute bieten moderne Sprachen wie Gleam eingebaute oder externe Bibliotheken an. Gleam's `httpc` Modul ist ein Wrapper um `erlang`'s `httpc`, erweitert um typsichere Funktionen. Das Erhalten des Webseiteninhalts erfolgt über einen GET-Request, der häufigste HTTP-Request zur Anforderung von Daten.

## See Also
- Gleam HTTP Dokumentation: https://hexdocs.pm/gleam_stdlib/gleam/http/
- Erlang's `httpc` Modul: https://erlang.org/doc/man/httpc.html
- Rust's `reqwest` für einen Vergleich: https://docs.rs/reqwest/