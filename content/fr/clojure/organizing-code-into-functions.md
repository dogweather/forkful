---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:09:31.047152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Organiser le code en fonctions consiste à emballer des blocs de code qui accomplissent des tâches spécifiques. Faire cela rend votre code propre, plus facile à maintenir, et plus clair pour les autres développeurs à lire.

## Comment faire :

Les fonctions en Clojure sont définies avec `defn`, suivi d'un nom, de paramètres, et d'un corps. Voici un exemple rapide.

```Clojure
(defn saluer [nom]
  (str "Bonjour, " nom "!"))

(saluer "Alex") ; => "Bonjour, Alex!"
```

Supposons maintenant que nous voulions calculer l'aire d'un rectangle. Au lieu de tout mélanger, nous le séparons en deux fonctions :

```Clojure
(defn aire [longueur largeur]
  (* longueur largeur))

(defn afficher-aire [longueur largeur]
  (println "L'aire est :" (aire longueur largeur)))

(afficher-aire 3 4) ; => L'aire est : 12
```

## Plongée Profonde

Il fut un temps, les programmeurs enfonçaient toute leur logique dans un seul bloc. C'était hideux. Puis la programmation structurée est arrivée, et les fonctions sont devenues incontournables. En Clojure, chaque fonction est de première classe — vous pouvez les manipuler comme toute autre valeur.

Des alternatives ? Certains pourraient s'embrouiller avec des multi-méthodes ou des fonctions d'ordre supérieur, mais ce ne sont que des épices dans le ragoût des fonctions.

Tout est dans les détails d'une fonction : elles sont immuables en Clojure, rendant les confusions d'effets secondaires moins probables. Elles se basent fortement sur la récursivité au lieu des boucles typiques, ce qui s'intègre bien avec les paradigmes fonctionnels du langage.

## Voir Aussi

- Le guide propre à Clojure : https://clojure.org/guides/learn/functions
- Bases de la Programmation Fonctionnelle : https://www.braveclojure.com/core-functions-in-depth/
- Les conférences de Rich Hickey : https://changelog.com/posts/rich-hickeys-greatest-hits - pour une perspective sur la philosophie de Clojure.