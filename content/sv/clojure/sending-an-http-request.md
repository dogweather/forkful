---
title:                "Clojure: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är en essentiell del av att bygga webbapplikationer och kommunikation mellan klient och server. Det kan vara användbart för att hämta data från en webbtjänst eller uppdatera en databas. Så om du planerar att bygga en webbapplikation i Clojure, är det viktigt att veta hur man skickar HTTP-förfrågningar.

## Hur man gör det

För att skicka en HTTP-förfrågan i Clojure behöver vi använda funktionen ```clj-http.client/request```. Vi kan använda den för att göra GET-, POST-, PUT- eller DELETE- förfrågningar. Till exempel, för att hämta data från en extern webbplats, kan vi använda:

```Clojure
(require '[clj-http.client :as client])

(def response (client/request {:url "https://www.example.com"
                               :method :get}))

(:body response) ;; returnerar innehållet från den externa webbplatsen
```

I det här exemplet skapar vi en variabel ```response``` och använder sedan ```client/request``` för att göra en GET-förfrågan till en valfri webbadress. Vi kan sedan använda ```(:body response)``` för att hämta innehållet från den externa webbplatsen.

För att skicka en POST-förfrågan med data, kan vi använda:

```Clojure
(def response (client/request {:url "https://www.example.com/submit"
                               :method :post
                               :body "name=John&age=30"}))
```

I det här fallet skickar vi med en body med namn och ålder som parametrar till den externa webbplatsen.

## Djupdykning

Nu när vi vet hur vi skickar HTTP-förfrågningar i Clojure, låt oss titta på några saker som vi kan anpassa. Funktionen ```client/request``` tar flera nyckelvärdespar som argument, inklusive ```:params``` för att skicka med eventuella sökparametrar, ```:headers``` för att skicka med eventuella rubriker, och ```:cookies``` för att skicka med cookie-data.

Vi kan också använda funktionen ```with-defaults``` från ```clj-http.client```, som låter oss ställa in standardvärden som används för alla efterföljande förfrågningar. Detta kan vara användbart för att till exempel sätta en bas-URL för alla våra förfrågningar.

## Se även

- [Dokumentation för clj-http](https://www.github.com/dakrone/clj-http)
- [En handledning för att skicka HTTP-förfrågningar i Clojure](https://blog.cognitect.com/blog/2016/1/14/clojure-http-client-does-not-http-kit)