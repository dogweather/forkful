---
title:                "Envoyer une requête http"
html_title:           "Clojure: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en quête d'une façon efficace d'envoyer des requêtes HTTP dans vos projets, alors Clojure pourrait être la réponse à vos attentes. Grâce à sa syntaxe concise et sa puissante bibliothèque de fonctions, Clojure rend l'envoi de requêtes HTTP rapide et facile.

## Comment Faire

Pour envoyer une requête HTTP en Clojure, tout ce dont vous avez besoin est la bibliothèque standard `clojure.core` et la bibliothèque `clj-http`. Tout d'abord, importez les bibliothèques en utilisant `require` :

```Clojure
(require '[clojure.core :as core])
(require '[clj-http.client :as client])
```

Ensuite, utilisez la fonction `client/get` pour envoyer une requête GET à une URL spécifique. Par exemple, pour obtenir le code de statut d'une requête à Google.com, vous pouvez écrire :

```Clojure
(client/get "https://www.google.com/")
;; => {:status 200, :headers {...}, :body "HTML content here"}
```

Vous pouvez également spécifier des paramètres supplémentaires dans la requête, tels que des headers et des paramètres de requête, en utilisant les arguments optionnels de la fonction `get` :

```Clojure
(client/get "https://www.google.com/"
            {:headers {"Content-Type" "application/json"}
             :query-params {:q "clojure"}})
;; => {:status 200, :headers {...}, :body "HTML content here"}
```

## Plongée Profonde

La bibliothèque `clj-http` offre un large éventail de fonctions pour envoyer des requêtes HTTP, y compris POST, PUT, DELETE et plus encore. Elle vous permet également de spécifier des options avancées telles que l'authentification, les cookies et les requêtes asynchrones.

De plus, la fonction `get` renvoie un map contenant des informations détaillées sur la réponse de la requête, notamment le code de statut, les headers et le contenu HTML. Cela vous permet de traiter facilement les réponses de vos requêtes pour en extraire les données nécessaires.

## Voir Aussi

- Documentation officielle de `clj-http` : https://github.com/dakrone/clj-http
- Tutoriel vidéo sur l'utilisation de `clj-http` : https://www.youtube.com/watch?v=Zc1WgJqz55U&t=16s