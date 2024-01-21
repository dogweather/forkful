---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:59:17.918755-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind der Weg, wie deine Anwendung mit dem Web spricht. Programmierer nutzen sie, um Daten zu holen, zu sendeln, und mit APIs zu interagieren.

## How to:
Clojure macht HTTP-Anfragen einfach mit der `clj-http`bibliothek. Installation: Leinigen `project.clj`:

```clojure
[clj-http "3.12.3"]
```

Einfache GET-Anfrage:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://api.example.com/data")]
  (println response))
```

Das könnte etwa so aussehen:

```clojure
{:status 200, :headers {...}, :body "..."}
```

POST-Anfrage mit Daten:

```clojure
(let [response (client/post "https://api.example.com/submit" 
                            {:form-params {:key "value"}})]
  (println response))
```

## Deep Dive:
`clj-http` ist eine Clojure-Bibliothek für HTTP-Anfragen, die auf Apache HttpClient aufbaut. Alternativen sind `http-kit` oder `aleph` für asynchrone Anfragen. Wichtig: `clj-http` unterstützt Synchrones und Asynchrones API, SSL und automatisches Codieren/Decodieren von JSON. Bei Anfragen wandelt die Bibliothek Clojure-Datenstrukturen in gültige HTTP-Anfragen um und umgekehrt.

## See Also:
- clj-http GitHub: https://github.com/dakrone/clj-http
- Apache HttpClient: http://hc.apache.org/httpcomponents-client-ga/index.html
- HTTP-Kit: http://www.http-kit.org/
- Aleph: https://aleph.io/