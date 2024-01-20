---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran är att skicka data till, eller begära data från, en server på interntet. Programmerare gör detta för att interagera med webbapplikationer, till exempel genom att hämta data från en databas eller ladda upp filer.

## Hur till:
För att skicka en HTTP-begäran i Clojure kan du använda det inbyggda biblioteket `clj-http`. Här är ett exempel på hur man gör en GET-begäran:

```Clojure
(require '[clj-http.client :as client])

(defn fetch-data []
  (let [response (client/get "http://example.com/api/data" {:as :json})]
    (println (:body response))))
```

Om du kör ovanstående kod, kommer serverns svar att skrivas ut i din terminal.

## Djupdykning
Historiskt sett har HTTP-begärningar använts sedan internets tidigaste dagar för att hämta HTML-dokument. Numera används de för mycket mer, som att interagera med RESTful API:er.

Ett alternativ till `clj-http` i Clojure är `http-kit`, som kan vara ett bra alternativ om du behöver hantera websockets eller behöver en snabbare klient.

En viktig detalj att notera när du skickar HTTP-begärningar är att du måste hantera svarskoder korrekt. Om du till exempel får ett 500-svar måste din kod kunna hantera detta på ett meningsfullt sätt.

## Se också
För mer information om hur man skickar HTTP-begärningar i Clojure, se följande källor:

1. Clj-http dokumentation: https://github.com/dakrone/clj-http
2. Http-kit dokumentation: https://github.com/http-kit/http-kit
3. Clojure webbprogrammering: https://clojure.org/guides/web_programming
4. Hantera HTTP-begärningar i Clojure - https://purelyfunctional.tv/guide/clojure-http-client/