---
title:                "Clojure: Recherche et remplacement de texte"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation. Cela peut être utile pour corriger des erreurs, modifier du code existant ou créer des outils de manutention de données. Avec le langage Clojure, vous pouvez facilement mettre en place des solutions efficaces pour ces tâches.

## Comment faire

Voici un exemple de code Clojure pour rechercher et remplacer une chaîne de caractères dans une liste :

```Clojure
(def data ["Hello" "Bonjour" "Hola"])

(println data)
;; Sortie: ["Hello" "Bonjour" "Hola"]

(def data (replace "Bonjour" "Salut" data))

(println data)
;; Sortie: ["Hello" "Salut" "Hola"]
```

Dans cet exemple, nous avons créé une liste de salutations en différentes langues, puis nous avons remplacé "Bonjour" par "Salut". En utilisant la fonction `replace`, nous pouvons facilement effectuer des recherches et des remplacements dans une liste ou une chaîne de caractères.

## Plongée profonde

Il existe différentes manières de rechercher et de remplacer du texte en utilisant Clojure, en fonction de vos besoins spécifiques. Vous pouvez utiliser des expressions régulières pour des recherches plus avancées ou des fonctions telles que `replace-first` ou `replace-nth` pour cibler des occurrences spécifiques dans une liste ou une chaîne de caractères.

Il est également important de noter que les recherches et remplacements peuvent être effectués de manière immuable, c'est-à-dire en créant une copie modifiée de la structure de données d'origine plutôt que de la modifier directement. Cela peut être préférable selon le contexte de votre application.

## Voir aussi

- [Documentation officielle Clojure](https://clojure.org)
- [Guide de référence pour les fonctions de recherche et remplacement Clojure](https://clojuredocs.org/clojure_string/string_functions)
- [Exemples de code pour la manipulation avancée de données en Clojure](https://github.com/emmanuelprat/data-manipulation-with-clojure)