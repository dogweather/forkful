---
title:                "Skicka en http-begäran"
html_title:           "Clojure: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför man skulle vilja skicka en HTTP förfrågan. En anledning kan vara att få data från en webbserver, till exempel för att hämta information från en API eller ladda ner en fil.

## Så här gör du

För att skicka en HTTP förfrågan i Clojure kan du använda funktionen "clj-http.client/request" från biblioteket "clj-http". För att använda det behöver du först importera biblioteket:

```Clojure
(ns min-projekt.core
  (:require [clj-http.client :as http]))
```

Sedan kan du använda funktionen, till exempel för att hämta data från en API. I det här exemplet visar vi hur du kan hämta användardata från GitHub API:

```Clojure
(def resultat (http/request
                {:method :get
                 :url "https://api.github.com/users/joakimkarud"
                 :headers {"Accept" "application/json"}}))
```

Outputen blir en datastruktur som innehåller all information från förfrågan. För att komma åt specifik data, till exempel användarens namn, kan du använda clojure.core/get:

```Clojure
(get resultat :body :login) ;; Returnerar "joakimkarud"
```

## Djupdykning

När du skickar en HTTP förfrågan finns det flera parametrar som du kan använda för att anpassa din förfrågan. Här är några av de vanligaste:

- :method: anger vilken typ av förfrågan du skickar, till exempel :get, :post eller :delete.
- :url: är den URL som du skickar förfrågan till.
- :headers: innehåller de headers som du vill skicka med förfrågan, som exempelvis "Accept" för att specificera vilket dataformat du vill ha tillbaka.
- :body: är den data som du vill skicka med din förfrågan, till exempel en JSON-kodad sträng.

Det finns också andra, mer avancerade, parametrar som du kan använda beroende på dina behov.

## Se även

- [Clojure API för HTTP förfrågningar](https://github.com/clj-http/clj-http)
- [GitHub API dokumentation](https://developer.github.com/v3/)
- [En tutorial om HTTP förfrågningar i Clojure](https://www.giddyup.co.za/clojure-http-client-tutorial/)