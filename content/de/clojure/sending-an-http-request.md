---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

#HTTP-Anfragen in Clojure senden: Ein schneller Überblick

## Was & Warum?

HTTP-Anfragen sind Bestandteil der Kommunikation zwischen Client und Server im Web. Als Programmierer schicken Sie diese, um Daten zu erhalten, zu senden, zu löschen oder zu aktualisieren.

## Anleitung

In Clojure können wir die Bibliothek `clj-http` zur Sendung von HTTP-Anfragen verwenden:

```Clojure
;; Fügen Sie in der Lein Projekt.clj-Datei die Abhängigkeit hinzu
[clj-http "3.10.1"]
```

Starten Sie nun den REPL und importieren Sie die Bibliothek:

```Clojure
(require '[clj-http.client :as client])
```

Eine einfache GET-Anfrage sieht so aus:

```Clojure
(client/get "http://example.com")
```

Die Ausgabe sollte ungefähr so aussehen:

```Clojure
{:status 200, :headers {"Content-Type" "text/html"}, ...}
```

## Vertiefung

Erste HTTP-Anfragen wurden Anfang der 90er-Jahre im Kontext der Webentwicklung eingeführt. Alternativ zu `clj-http` gibt es in Clojure andere Bibliotheken wie `http-kit` oder `aleph`. Die Implementierung von `clj-http` beruht auf der Java-Bibliothek Apache HttpClient, was sie sehr leistungsfähig und flexibel macht.

## Weiterführende Informationen

Weitere nützliche Informationen und Beispiele finden Sie in der [offiziellen clj-http-Dokumentation](https://github.com/dakrone/clj-http) und in diesem [Blog-Post](https://www.baeldung.com/clojure-http-requests) über HTTP-Anfragen in Clojure.