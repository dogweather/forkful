---
title:                "Versenden einer http-Anfrage"
html_title:           "Clojure: Versenden einer http-Anfrage"
simple_title:         "Versenden einer http-Anfrage"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was ist das und warum?
Sich eine HTTP-Anfrage zu schicken, ist ein wichtiger Bestandteil der Programmierung. Es erlaubt uns, Daten aus dem Internet zu erhalten und mit externen APIs zu interagieren. Wir senden Anfragen, um Informationen zu erhalten, die wir dann in unsere Programme integrieren können.

## Wie geht es?
Um eine HTTP-Anfrage in Clojure zu machen, können wir die "clj-http" Bibliothek verwenden. Mit dieser Bibliothek können wir eine Anfrage an eine URL senden und erhalten eine Antwort zurück. Hier ist ein Beispiel:

```Clojure
(ns example.core
  (:require [clj-http.client :refer [get]]))

(defn send-request []
  (let [response (get "https://example.com")]
    (:body response)))
```

Das Ergebnis dieser Funktion wäre der Inhalt der Webseite, die wir angefragt haben.

## Tiefer tauchen
Historisch gesehen waren HTTP-Anfragen eine oft komplexe und zeitaufwändige Aufgabe für Programmierer. Aber mit der Entwicklung von Bibliotheken wie "clj-http" ist es jetzt viel einfacher geworden. Es gibt auch andere Bibliotheken, wie zum Beispiel "http-kit" und "core.async", die für diese Aufgabe verwendet werden können.

Für die Implementierung einer HTTP-Anfrage in Clojure gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen wir sicherstellen, dass wir die richtigen Datenstrukturen verwenden, um die Anfrage und die Antwort zu verwalten. Auch sollten wir sicherstellen, dass wir mit Ausnahmen umgehen, falls etwas schief geht.

## Siehe auch
- Offizielle "clj-http" Bibliothek: https://github.com/dakrone/clj-http
- "http-kit" Bibliothek: https://github.com/http-kit/http-kit
- "core.async" Bibliothek: https://github.com/clojure/core.async