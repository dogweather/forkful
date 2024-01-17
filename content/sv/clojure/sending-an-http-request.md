---
title:                "Sända en http-begäran"
html_title:           "Clojure: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan är ett sätt för en programmerare att anropa en extern resurs, som en webbserver, för att få eller skicka information. Det kan användas för att hämta data från en webbplats, skicka formulärdata eller kommunicera med ett API. Det är ett integrerat verktyg för webbutvecklare och en viktig del av internetinfrastrukturen.

## Så här gör du:

```Clojure
(require '[clj-http.client :as client])
```
För att skicka en HTTP GET-förfrågan med Clojure, använder vi funktionen `client/get` och anger den URL som vi vill anropa. Detta returnerar en respons som innehåller statuskod, headers och kroppen på svaret.
```Clojure
(client/get "https://example.com")

=> {:status 200, :headers {"Content-Type" "text/html"}, :body "<html>...</html>"}
```

Om vi vill skicka en POST-förfrågan och skicka med data i kroppen kan vi använda funktionen `client/post` och ange en `:body` parameter.
```Clojure
(client/post "https://example.com/api" {:body "name=John&age=30"})

=> {:status 200, :headers {"Content-Type" "application/json"}, :body "{\"message\":\"User created\"}"}
```

## Djupdykning
HTTP, eller Hypertext Transfer Protocol, är ett protokoll som används för att överföra data över internet. Det infördes redan 1991 av Tim Berners-Lee, som också skapade World Wide Web. Innan dess användes andra protokoll som FTP och Gopher för att överföra data.

Det finns många alternativ till Clojure för att skicka HTTP-förfrågningar, som t.ex. Java-biblioteket Apache HttpComponents eller JavaScript-biblioteket Axios. Men Clojure har den fördelen att det är lätt att integrera med andra delar av Clojure-ekosystemet, som ring, för att hantera HTTP-förfrågningar i en webbapplikation.

När det gäller implementationen av HTTP-förfrågningar i Clojure, använder det sig av Java-biblioteket HttpCore för att hantera kommunikationen med webbservern. Detta betyder att Clojure-utvecklare får tillgång till en kraftfull och stabil implementation av HTTP.

## Se även
- [HTTP - Wikipedia](https://sv.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [Clj-http Dokumentation](https://github.com/dakrone/clj-http)
- [Ring - Clojure Web Application Library](https://github.com/ring-clojure/ring)