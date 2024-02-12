---
title:                "Envoi d'une requête HTTP"
aliases: - /fr/clojure/sending-an-http-request.md
date:                  2024-01-20T17:59:11.431152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP, c'est comme envoyer une lettre numérique à un serveur web pour obtenir des données ou déclencher une action. Les programmeurs le font pour communiquer avec des API ou des services web, pour récupérer des informations ou envoyer des instructions.

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
