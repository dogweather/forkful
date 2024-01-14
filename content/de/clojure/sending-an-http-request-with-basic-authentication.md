---
title:                "Clojure: Versenden einer HTTP-Anfrage mit grundlegendem Authentifizierung"
simple_title:         "Versenden einer HTTP-Anfrage mit grundlegendem Authentifizierung"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum
Das Senden von HTTP-Anfragen mit Basisauthentifizierung kann in vielen Programmierprojekten nützlich sein, insbesondere wenn eine sichere Übertragung von Daten erforderlich ist.

## Wie geht man vor
Um eine HTTP-Anfrage mit Basisauthentifizierung in Clojure zu senden, müssen Sie zuerst die Bibliothek `clj-http` importieren. Dann können Sie die Funktion `clj-http.client/post` verwenden, um eine Post-Anfrage an eine bestimmte URL zu senden und die Basisauthentifizierungsinformationen anzugeben.

```Clojure
(import '(clj-http.client :as http))

(def url "https://example.com/api")
(def credentials {:basic-auth ["username" "password"]})

(http/post url {:basic-auth credentials})
```

Die oben genannten Zeilen senden eine HTTP-Anfrage mit Basisauthentifizierung an die angegebene URL. Sie können auch verschiedene Parameter wie Header oder Daten hinzufügen, je nach den spezifischen Anforderungen Ihres Projekts.

Das Ergebnis der Anfrage wird in einem Clojure-Map-Format zurückgegeben. Hier ist ein Beispiel für ein mögliches Ergebnis:

```Clojure
{:status 200
 :headers {"Content-Type" "application/json"}
 :body "{\"message\": \"SUCCESS\", \"data\": {\"id\": 12345}}" 
 :trace-redirects ["https://example.com/api"]}
```

## Tiefentauchen
Für eine genaue Anleitung und detailliertere Informationen zu den verschiedenen optionieren Parametern und Einstellungen für das Senden von HTTP-Anfragen mit Basisauthentifizierung in Clojure, sollten Sie die offizielle Dokumentation der `clj-http` Bibliothek lesen.

## Siehe auch
- [Offizielle Dokumentation von clj-http](https://github.com/dakrone/clj-http)
- [Beispielprojekt für clojure-http mit Basisauthentifizierung](https://github.com/example-project)
- [Einführung in Clojure für HTTP-Anfragen](https://clojure.org/reference/java_interop#_making_http_requests)