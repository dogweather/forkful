---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Démarrer un nouveau projet de programmation en Clojure c'est comme peindre une toile vierge: c'est l'opportunité de créer quelque chose de unique à partir de zéro. Les programmeurs le font pour résoudre des problèmes, explorer de nouvelles idées, ou simplement pratiquer leurs compétences.

## Comment faire:
Commençons par installer [Leiningen](https://leiningen.org/#install) - un outil populaire pour gérer les projets Clojure. Ensuite, vous pouvez créer un nouveau projet Clojure avec la commande:

```clojure
lein new mon-projet
```

Ce qui génère un répertoire de projet avec une structure de fichiers de base. Vous pouvez compiler et exécuter le projet avec:

```clojure
lein run
```

Dans un fichier de source, vous pouvez écrire une fonction comme celle-ci:

```clojure
(defn saluer [nom]
  (str "Bonjour, " nom "!"))
```

Et l'appeler en utilisant:

```clojure
(saluer "Pierre") => "Bonjour, Pierre!"
```

## Deep Dive:
Clojure a été créé en 2007 par Rich Hickey comme une version moderne de Lisp pour la JVM. C'est une solution efficace pour ceux qui aiment le style de programmation fonctionnel et qui ont besoin de la puissance de la JVM.

Alternativement, vous pouvez utiliser des outils comme `boot` ou `depstar` pour gérer vos projets Clojure mais Leiningen reste l'outil le plus largement utilisé.

Lorsqu'on commence un nouveau projet, Leiningen génère une structure de fichiers de base qui as un fichier `project.clj`. Ce fichier contient des informations sur votre projet, y compris les dépendances requises.

## Voir Aussi:
Pour plus d'informations, consultez ces ressources:

- [Documentation officielle de Clojure](https://clojure.org/guides/getting_started)
- [Clojure for the Brave and True (livre en ligne gratuit)](https://www.braveclojure.com/)