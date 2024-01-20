---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Envoyer une requête HTTP en Clojure: Un Aperçu

## Quoi & Pourquoi?
Une requête HTTP, c'est un moyen standard de demander des informations à un serveur web. Les programmeurs envoient des requêtes HTTP pour interagir avec des API web, récupérer des données, soumettre des formulaires, etc.

## Comment faire:
Voici un exemple simple de comment envoyer une requête GET en utilisant la libraire `clj-http` en Clojure.

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com" {:as :json})]
  (println (:status response))
  (println (:body response))
)
```

Cela enverra une requête GET à `http://example.com` et imprimera le statut de la réponse et le corps de la réponse.

## Plongée en profondeur
La capacité d'envoyer des requêtes HTTP a été prise en charge dans Clojure depuis ses débuts, reflétant le rôle important que joue HTTP dans la programmation moderne. 

Des alternatives à `clj-http` existent, y compris `http-kit` et `aleph`, chacun ayant ses propres forces. Par exemple, `http-kit` est connu pour sa rapidité, tandis que `aleph` offre une abstraction de niveau supérieur utile pour travailler avec des modèles de programmation basés sur des flux de données.

Lors de l'envoi d'une requête HTTP, il y a plusieurs détails d'implémentation à garder à l'esprit. Par exemple, la gestion des erreurs réseau, la définition du type de contenu (content type) et de l'en-tête d'acceptation (accept header), et la manipulation correcte de la fermeture de connexion.

## Voir Aussi
- [clj-http GitHub](https://github.com/dakrone/clj-http)
- [http-kit GitHub](https://github.com/http-kit/http-kit)
- [aleph GitHub](https://github.com/ztellman/aleph)
- [HTTP dans Clojure - Guide pratique](https://practicalli.github.io/clojure-webapps/http-in-clojure.html)