---
date: 2024-01-20 18:01:28.277583-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-03-13T22:44:34.994337-06:00'
model: gpt-4-1106-preview
summary: .
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to: (Jak to zrobić:)
```Clojure
(require '[clj-http.client :as client])

(defn fetch-secure-resource [url username password]
  (let [credentials (str username ":" password)
        encoded-credentials (.toString (java.util.Base64/getEncoder (java.nio.charset.StandardCharsets/UTF_8) (.getBytes credentials)))]
    {:status (:status (client/get url {:basic-auth [username password]}))
     :body   (:body (client/get url {:headers {"Authorization" (str "Basic " encoded-credentials)}}))}))

(println (fetch-secure-resource "http://secured-website.com/resource" "myUser" "myPass"))
```
Sample output:
```
{:status 200, :body "Here is your secure content."}
```

## Deep Dive (Dogłębny wgląd):
Zarządzanie accessem jest stare jak Internet. W początkowych dniach, pojawiała się Basic Authentication - prosta lecz najsłabsza zabezpieczeń. Mimo to, ciągle jest używana, zwykle gdy trzeba coś prototypować. Alternatywy obejmują OAuth, JWT i inne bardziej złożone systemy. W Clojure, wysyłanie żądania HTTP z autoryzacją Basic wymaga kodowania 'login:hasło' w Base64 i dołączenia tego do nagłówków żądania - to garść linijek, które robią robotę. Oczywiście, jeśli potrzebujesz więcej, biblioteki jak `clj-http` oferują wbudowane opcje, by to uprościć.

## See Also (Zobacz także):
- [clj-http: A Clojure HTTP library](https://github.com/dakrone/clj-http)
- [Clojure - Getting Started](https://clojure.org/guides/getting_started)
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [ClojureDocs - A community-powered documentation and examples repository for Clojure](https://clojuredocs.org/)
