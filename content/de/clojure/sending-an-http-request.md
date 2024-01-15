---
title:                "Eine http-Anfrage senden"
html_title:           "Clojure: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Senden von HTTP-Anfragen auseinandersetzen? Nun, da fast jede moderne Anwendung eine Verbindung zum Internet benötigt, ist es unerlässlich zu wissen, wie man HTTP-Anfragen sendet, um Daten von entfernten Servern abzurufen.

## How To

Es gibt mehrere Möglichkeiten, in Clojure HTTP-Anfragen zu senden. Hier ist eine einfache Methode, um eine GET-Anfrage an eine URL zu senden und die Antwort zu erhalten:

```Clojure
(require '[clojure.data.json :as json])
(require '[clj-http.client :as http])
 
(defn get-data [url]
  (let [response (http/get url)]
    (-> response
        :body
        (json/read-str :key-fn keyword)
        )))
```

In diesem Beispiel importieren wir zunächst die Bibliotheken `clojure.data.json` und `clj-http.client`, die uns beim Umgang mit JSON und HTTP-Anfragen helfen werden. Dann definieren wir die Funktion `get-data`, die eine URL als Parameter erhält und mithilfe der Funktion `http/get` eine GET-Anfrage an diese URL sendet. Die Antwort wird als JSON gelesen und als Clojure-Datenstruktur zurückgegeben.

Um das Ergebnis dieser Funktion zu sehen, können wir sie wie folgt aufrufen:

```Clojure
(get-data "https://jsonplaceholder.typicode.com/posts")
```

Die Ausgabe sieht dann wie folgt aus:

```Clojure
[{ :userId 1
   :id 1
   :title "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
   :body "quia et suscipit" }
 { :userId 1
   :id 2
   :title "qui est esse"
   :body "est rerum tempore" }
...
]
```

## Deep Dive

Wenn wir genauer hinschauen, können wir feststellen, dass die Funktion `get-data` mehrere optionalen Parameter akzeptiert, um die HTTP-Anfrage anzupassen. Zum Beispiel können wir einen Header oder eine Authentifizierung hinzufügen:

```Clojure
(defn get-data [url]
  (let [response (http/get url {:headers {"Authorization" "ApiKey xyz"}})]
    (-> response
        :body
        (json/read-str :key-fn keyword)
        )))
```

Wir können auch andere HTTP-Methoden wie POST, PUT oder DELETE verwenden, indem wir den entsprechenden Parameter in die `http/get`-Funktion einfügen. Clojure bietet auch die Möglichkeit, asynchrone HTTP-Anfragen zu senden, die nützlich sein können, wenn wir mehrere Anfragen parallel verarbeiten möchten.

## Siehe auch

* [Offizielle Dokumentation zu clj-http](https://github.com/dakrone/clj-http)
* [Tutorial zu HTTP-Verbindungen in Clojure](https://www.innoq.com/de/articles/2011/03/http-verbindungen-in-clojure/)