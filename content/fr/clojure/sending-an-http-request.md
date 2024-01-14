---
title:                "Clojure: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi de requêtes HTTP est un élément essentiel de la programmation web moderne. Cela permet aux applications de communiquer avec des serveurs distants et d'accéder à des ressources en ligne. En utilisant Clojure, vous pouvez facilement envoyer des requêtes HTTP et manipuler les données qui en résultent.

## Comment Faire

Pour envoyer une requête HTTP en utilisant Clojure, vous pouvez utiliser la fonction `clojure.http.client/request`. Voici un exemple de code qui envoie une requête GET à l'URL "https://www.example.com" et imprime le corps de la réponse :

```Clojure
(let [response (clojure.http.client/request {:method :get :url "https://www.example.com"})]
  (println (:body response)))
```

Ceci produira la sortie suivante :

```Clojure
<html>
  <head>
    <title>Example Domain</title>
  </head>
  <body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
  </body>
</html>
```

Vous pouvez également spécifier des paramètres supplémentaires dans la requête, tels que des en-têtes et des paramètres de requête. Pour plus d'informations sur l'utilisation de la fonction `request`, vous pouvez consulter la documentation officielle de Clojure.

## Plongée Profonde

En utilisant Clojure, vous pouvez également envoyer des requêtes HTTP asynchrones en utilisant la fonction `clojure.http.client/async-request`. Cela peut être utile pour améliorer les performances de votre application en envoyant plusieurs requêtes en même temps.

De plus, Clojure fournit plusieurs bibliothèques tierces telles que `clj-http` et `http-kit` qui offrent des fonctionnalités supplémentaires pour gérer les requêtes HTTP.

## Voir Aussi

- [Documentation officielle sur les requêtes HTTP en Clojure](https://clojure.github.io/clojure/clojure.http.client-api.html)
- [Exemple de tutoriel sur l'utilisation de HttpKit en Clojure](https://luminusweb.com/docs/http-kit)
- [Guide complet sur la manipulation des requêtes HTTP en Clojure](https://www.vojtechruzicka.com/http-requests-clojure-with-clj-http/)