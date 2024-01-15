---
title:                "Telecharger une page web"
html_title:           "Clojure: Telecharger une page web"
simple_title:         "Telecharger une page web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes intéressé par l'informatique, vous avez sûrement entendu parler du langage de programmation Clojure. Ce langage fonctionnel et dynamique offre de nombreuses possibilités pour développer des applications web. Dans cet article, nous allons vous montrer comment utiliser Clojure pour télécharger une page web et en extraire des données. 

## Comment faire 

Tout d'abord, vous aurez besoin d'installer Clojure sur votre ordinateur. Vous pouvez suivre ce [guide](https://clojure.org/guides/getting_started) pour l'installation. Une fois que vous avez terminé l'installation, vous pouvez ouvrir un terminal et lancer la console Clojure en tapant `clojure` et en appuyant sur Entrée. Maintenant, vous êtes prêt à commencer ! 

Nous allons utiliser la bibliothèque "clj-http" pour télécharger une page web. Vous pouvez l'ajouter à votre projet en utilisant Leiningen ou en téléchargeant directement le JAR depuis [ici](https://github.com/dakrone/clj-http). Une fois que vous avez ajouté la bibliothèque à votre projet, vous pouvez l'importer dans votre code en ajoutant `(require '[clj-http.client :as client])` en haut de votre fichier. 

Maintenant, pour télécharger une page web, vous pouvez utiliser la fonction `client/get`, en lui passant l'URL de la page en tant que premier argument. Par exemple, si vous voulez télécharger la page d'accueil de Google, vous pouvez faire : 

```clojure
(def page (client/get "https://www.google.com"))
```

Ensuite, pour extraire des données de cette page, nous allons utiliser la bibliothèque "enlive". Vous pouvez l'ajouter à votre projet en utilisant Leiningen ou en téléchargeant directement le JAR depuis [ici](https://github.com/cgrand/enlive). Ensuite, vous pouvez l'importer dans votre code en ajoutant `(require '[net.cgrand.enlive-html :as enlive])`. 

Maintenant, nous allons utiliser la fonction `enlive/parse` pour convertir le contenu de la page HTML en une structure de données Clojure. Vous pouvez ensuite utiliser des sélecteurs CSS pour extraire les éléments que vous souhaitez. Par exemple, pour extraire tous les liens de la page Google, vous pouvez faire : 

```clojure
(def links (enlive/select (enlive/parse page) [:a]))
```

Vous pouvez également utiliser des sélecteurs plus précis pour extraire des données spécifiques. Une fois que vous avez sélectionné les données souhaitées, vous pouvez les manipuler et les utiliser à votre guise. 

## Plongée en profondeur 

En utilisant Clojure pour télécharger et manipuler une page web, vous avez un contrôle total sur les données que vous récupérez. Vous n'êtes pas limité par les fonctionnalités fournies par les navigateurs web traditionnels, vous pouvez donc créer des outils personnalisés pour extraire des données de n'importe quelle page web. De plus, en utilisant des librairies telles que "clj-http" et "enlive", vous pouvez facilement étendre vos fonctionnalités pour répondre à tous vos besoins. 

## Voir aussi 

- [Guide d'installation Clojure](https://clojure.org/guides/getting_started)
- [Bibliothèque clj-http](https://github.com/dakrone/clj-http)
- [Bibliothèque enlive](https://github.com/cgrand/enlive)