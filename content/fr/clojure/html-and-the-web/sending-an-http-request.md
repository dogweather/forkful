---
date: 2024-01-20 17:59:11.431152-07:00
description: "Comment faire : Historiquement, les requ\xEAtes HTTP \xE9taient manuelles\
  \ et complexes. Avec Clojure, clj-http simplifie l'envoi de requ\xEAtes HTTP en\
  \ encapsulant\u2026"
lastmod: '2024-04-05T21:53:58.859638-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, les requ\xEAtes HTTP \xE9taient manuelles et complexes."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## Comment faire :
```Clojure
;; Add clj-http to your project dependencies
[clj-http "3.12.3"]

;; Import the library
(require '[clj-http.client :as client])

;; Make a simple GET request
(def response (client/get "http://httpbin.org/get"))

;; Print the status code and body of the response
(println (:status response) (:body response))

;; Output:
;; 200
;; {
;;   "args": {}, 
;;   "headers": {
;;     "Accept": "...", 
;;     "Host": "httpbin.org",
;;     ...
;;   }, 
;;   "origin": "...", 
;;   "url": "https://httpbin.org/get"
;; }
```

## Plongée Profonde :
Historiquement, les requêtes HTTP étaient manuelles et complexes. Avec Clojure, clj-http simplifie l'envoi de requêtes HTTP en encapsulant les détails derrière une simple API. 

Alternatives ? Il existe d'autres bibliothèques comme Aleph ou http-kit, chacune avec ses propres avantages. Aleph est basé sur Netty et conçu pour les performances, tandis que http-kit est petit et rapide, souvent privilégié pour les petits services.

Détails d'implémentation : clj-http est construit sur Apache HttpComponents, offrant une compatibilité étendue et une gestion robuste des connexions. Il permet des requêtes synchrones et asynchrones, la personnalisation des entêtes HTTP, et la gestion des cookies, entre autres.

## À Voir Aussi :
- Documentation clj-http : [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Http-kit : [http://www.http-kit.org/](http://www.http-kit.org/)
- Aleph : [https://aleph.io/](https://aleph.io/)
