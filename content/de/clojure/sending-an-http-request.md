---
title:                "Clojure: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Senden von HTTP-Anfragen beschäftigen? Nun, HTTP-Anfragen sind der grundlegende Mechanismus für den Austausch von Daten im Web. Das Verständnis dieser Funktionalität ermöglicht es Ihnen, effiziente und leistungsstarke Anwendungen zu entwickeln, die nahtlos mit anderen Webdiensten kommunizieren können.

## Wie geht man vor?

### Einführung in HTTP-Anfragen

Die einfachste Möglichkeit, eine HTTP-Anfrage mit Clojure zu senden, ist die Verwendung der `clojure.java.io` Bibliothek. Als erstes importieren wir diese Bibliothek mit:

```Clojure
(require '[clojure.java.io :as io])
```

Als nächstes können wir mit der Funktion `url` eine URL erstellen, auf die wir unsere Anfrage senden möchten:

```Clojure
(def url (url "https://www.example.com"))
```

Jetzt können wir die HTTP-Anfrage mit der Funktion `input-stream` senden und die Antwort mit `slurp` erhalten:

```Clojure
(with-open [response (io/input-stream url)]
  (slurp response))
```

Diese Funktionen bilden die Basis für den Senden einer einfachen HTTP-Anfrage. Jetzt können wir tiefer in die verschiedenen Optionen für HTTP-Anfragen eintauchen.

### Erweiterte Optionen für HTTP-Anfragen

Die `clojure.java.io` Bibliothek verfügt über viele Optionen, um HTTP-Anfragen anzupassen, wie z.B. das Hinzufügen von Anfrageparametern, die Angabe von Anfrageheader und die Verwendung verschiedener Methoden wie GET, POST, PUT, etc.

Um beispielsweise eine POST-Anfrage mit parametrisierten Daten zu senden, könnten wir folgenden Code verwenden:

```Clojure
(io/request url
  :method "POST"
  :data {"username" "John" "password" "12345"})
```

Diese Funktion würde eine HTTP-POST-Anfrage an die angegebene URL senden und die im `:data`-Parameter angegebenen Daten mitschicken.

## Tiefer gehender Einblick

Das Senden von HTTP-Anfragen ist ein komplexes Thema und es gibt viele Dinge, die man beachten sollte, um effektive Anfragen zu senden und fehlerhafte Antworten zu behandeln. Hier sind einige Ressourcen, die Ihnen helfen können, ein tieferes Verständnis für HTTP-Anfragen in Clojure zu erlangen:

- [Offizielle Dokumentation zu `clojure.java.io`](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Eine umfassende Anleitung zur Verwendung von HTTP-Anfragen in Clojure](https://www.beyondthelines.net/computing/http-calls-in-clojure/)
- [Offizielles Clojure Cookbook: Verwenden von HTTP-Anfragen](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/09_networking/9-12_working-with-http-requests.asciidoc)

## Siehe auch

- [Offizielle Clojure-Dokumentation](https://clojure.org/)
- [Eine Einführung in Clojure für Anfänger](https://www.freecodecamp.org/news/clojure-for-the-brave-and-true/)