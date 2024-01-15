---
title:                "Envoyer une demande http avec une authentification de base"
html_title:           "Clojure: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer des requêtes HTTP avec une authentification basique peut être utile lorsqu'on souhaite protéger l'accès à une ressource ou à un service en ligne. Cela permet de s'assurer que seuls les utilisateurs authentifiés peuvent accéder à ces ressources.

## Comment Faire

```Clojure
(require '[clj-http.client :as client])
;; Importer la bibliothèque clj-http.client pour pouvoir utiliser les fonctions HTTP
```

```Clojure
(def login "nom_utilisateur")
(def password "mot_de_passe")
(def url "https://exemple.com/ressource")

(client/get url {:basic-auth [login password]})
;; Cette requête utilise la fonction "get" pour récupérer une ressource, en fournissant les informations de connexion via l'option "basic-auth"
```

Output:

```Clojure
(def logged-in-response {:status 200, 
                         :headers {"content-type" "text/plain"}, 
                         :body "Vous êtes maintenant connecté !"})

;; Ceci est la réponse obtenue après une requête réussie avec authentification basique.
```

## Deep Dive

L'authentification basique utilise une méthode d'authentification simple basée sur l'envoi d'un nom d'utilisateur et d'un mot de passe en clair. Cela pose des problèmes de sécurité car ces informations peuvent être interceptées. Pour une meilleure sécurité, il est recommandé d'utiliser des méthodes d'authentification plus avancées telles que l'authentification par jeton.

Néanmoins, l'authentification basique reste utile dans certains cas où la sécurité n'est pas une préoccupation majeure, par exemple lorsqu'on souhaite protéger l'accès à des ressources internes dans une entreprise.

## Voir Aussi

- [Documentation officielle de clj-http](https://github.com/dakrone/clj-http)
- [Tutoriel sur l'authentification basique avec clj-http](https://www.cloudbees.com/blog/using-basic-authentication-clojure)
- [Article sur la sécurité en HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/securite)