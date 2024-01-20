---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Télécharger une page Web en utilisant Clojure

## Quoi & Pourquoi ?

"Télécharger une page Web" est l'action d'extraire le contenu d'une page Web vers votre ordinateur pour un usage ultérieur. Les programmeurs le font généralement pour récupérer et analyser des données.

## Comment faire :

L'exemple suivant montre comment utiliser la bibliothèque `clj-http` pour télécharger une page web.

```Clojure
(ns web-download.core
  (:require [clj-http.client :as client]))

(defn get-web-page [url]
  (let [response (client/get url)]
    (:body response)))

(defn main []
  (println (get-web-page "https://www.google.com")))

(main)
```

Le code ci-dessus récupère le contenu de Google's main page.

## Deep Dive 

1. **Contexte historique**: Le téléchargement de pages Web a commencé lorsque le monde s'est rendu compte de l'immense quantité de données disponibles sur Internet.
2. **Alternatives**: D'autres bibliothèques comme `java.net.URL`, `java.nio.file` peuvent être utilisées pour télécharger une page Web. Chacune a ses propres avantages et inconvénients.
3. **Détails d'implémentation**: `clj-http` envoie une requête HTTP GET à l'URL spécifiée et renvoie la réponse. `:body` contient le contenu HTML de la page Web.

## Voir Aussi 

1. Documentation clj-http : [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
2. Pour analyser le HTML : [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)