---
title:                "Clojure: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de remplacer rapidement et efficacement du texte dans un programme pour différentes raisons telles que la correction d'erreurs ou la mise à jour de code. Heureusement, Clojure offre des outils puissants pour effectuer cette tâche de manière efficace et rapide.

## Comment faire

La fonction clé pour rechercher et remplacer du texte en Clojure est `clojure.string/replace`. Elle prend en paramètres une chaîne de caractères, le motif à rechercher et le motif de remplacement. Voici un exemple de code pour remplacer toutes les occurrences du mot "hello" par "bonjour" dans une chaîne de caractères :

```clojure
(clojure.string/replace "hello world!" "hello" "bonjour")
```

La sortie de ce code sera "bonjour world!". Vous pouvez également utiliser des expressions régulières comme motif de recherche pour une plus grande flexibilité. Par exemple, si vous voulez remplacer toutes les occurrences de nombres dans une chaîne de caractères par "*" vous pouvez utiliser le code suivant :

```clojure
(clojure.string/replace "il y a 5 personnes ici" #"\d+" "*")
```

La sortie de ce code sera "il y a * personnes ici". Vous pouvez également utiliser cette fonction pour remplacer des éléments dans une liste ou une séquence. Par exemple, si vous voulez remplacer tous les nombres impairs dans une liste par 0, vous pouvez utiliser le code suivant :

```clojure
(map #(if (odd? %) 0 %) [1 2 3 4 5])
```

La sortie de ce code sera (0 2 0 4 0).

## Plongée en profondeur

En plus de la fonction `replace`, Clojure offre également d'autres fonctions utiles pour rechercher et remplacer du texte telles que `clojure.string/replace-first` qui ne remplace que la première occurrence, `clojure.string/replace-last` qui ne remplace que la dernière occurrence et `clojure.string/replace-nth` qui ne remplace que la n-ième occurrence. De plus, les expressions régulières en Clojure sont basées sur l'API Java, ce qui offre de nombreuses possibilités avancées pour la recherche et le remplacement de texte.

## Voir aussi

- [Documentation officielle Clojure sur la fonction replace](https://clojuredocs.org/clojure.string/replace)
- [Documentation Java sur les expressions régulières](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)