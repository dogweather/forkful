---
title:                "Commencer un nouveau projet"
html_title:           "Clojure: Commencer un nouveau projet"
simple_title:         "Commencer un nouveau projet"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous aimez les langages fonctionnels et que vous souhaitez construire des applications robustes et concises, vous devriez certainement envisager d'utiliser Clojure pour votre prochain projet. Avec sa syntaxe élégante et son système de gestion des erreurs fiable, Clojure offre une expérience de codage agréable et efficace.

## Comment Faire

Pour démarrer un nouveau projet en Clojure, il vous suffit de suivre ces étapes simples :

1. Installez le [Clojure CLI Tools](https://clojure.org/guides/getting_started) en suivant les instructions de la documentation officielle.

2. Créez un nouveau projet en utilisant la commande `clojure -A:new`. Vous pouvez spécifier un nom pour votre projet et choisir un modèle de base (comme un projet web ou une application de ligne de commande).

3. Accédez au dossier de votre projet nouvellement créé et ouvrez le fichier `deps.edn` qui contient les dépendances de votre projet. Vous pouvez y ajouter des bibliothèques externes en utilisant le format `nom-de-la-bibliothèque/version`.

4. Créez un fichier `core.clj` qui servira de point d'entrée pour votre application. À partir de là, vous pouvez écrire du code en utilisant les nombreuses fonctions puissantes de Clojure, comme `map`, `filter` et `reduce`.

5. Utilisez la commande `clojure -A:test` pour exécuter les tests unitaires de votre projet et assurez-vous que tout fonctionne correctement.

6. Enfin, vous pouvez créer un exécutable JAR en utilisant la commande `clojure -A:uberjar` et distribuer votre application.

Voici un exemple de code qui montre comment utiliser la fonction `filter` pour trouver tous les nombres impairs d'une liste :

```Clojure
(def nums [1 2 3 4 5 6 7])
(def odds (filter odd? nums))
(println odds) ;; affiche [1 3 5 7]
```

## Plongeon en Profondeur

En plus des étapes ci-dessus, il y a d'autres aspects à prendre en compte lors du démarrage d'un nouveau projet en Clojure. Par exemple, vous pouvez utiliser le [framework web Ring](https://github.com/ring-clojure/ring) pour créer des applications web, ou [Leiningen](https://leiningen.org/) pour gérer et construire vos projets Clojure.

Vous pouvez également trouver utile d'apprendre les [fonctions de haute ordre](https://clojure.org/guides/getting_started#_functions_as_data) de Clojure, qui permettent de traiter les fonctions comme de simples valeurs.

Enfin, n'oubliez pas de consulter la [documentation officielle de Clojure](https://clojure.org/) pour plus d'informations et de ressources utiles.

## Voir Aussi

- [La documentation officielle de Clojure](https://clojure.org/)
- [Clojure - Site officiell](https://clojure.org/)
- [Clojure pour les curieux - Guide en français](https://github.com/l28etud/Clojure-pour-les-curieux)