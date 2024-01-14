---
title:    "Clojure: Trouver la longueur d'une chaîne de caractères"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Parfois, lors de l'écriture de code Clojure, il est nécessaire de trouver la longueur d'une chaîne de caractères. Cela peut sembler être une tâche simple, mais cela peut être utile dans de nombreuses situations telles que la manipulation de données, la validation de saisie utilisateur ou même l'affichage de résultats.

# Comment faire

Il existe plusieurs façons de trouver la longueur d'une chaîne de caractères en Clojure. Dans cet article, nous allons en explorer quelques-unes en utilisant des exemples de code dans des blocs de code ```Clojure ...```.

1. Utiliser la fonction `count`

```Clojure
(count "Bonjour") ; Retourne 7
(count "") ; Retourne 0
```

2. Utiliser l'opérateur `count`

```Clojure
(= 6 (count "Hello")) ; Retourne true
(= 0 (count "")) ; Retourne true
```

3. Utiliser la fonction `length`

```Clojure
(length "Bonjour") ; Retourne 7
(length "") ; Retourne 0
```

4. Utiliser la méthode `.length` sur un objet String Java

```Clojure
(.length "Hello") ; Retourne 5
(.length "") ; Retourne 0
```

# Plongée en profondeur

Si nous regardons de plus près la fonction `count`, nous pouvons voir qu'elle utilise un protocole appelé `Counted` qui fournit une implémentation de la fonction `count` pour tout type qui implémente ce protocole. Cela signifie que nous pouvons utiliser la fonction `count` non seulement sur des chaînes de caractères, mais aussi sur d'autres types tels que des listes, des vecteurs et des sets.

Une autre chose intéressante à noter est que la méthode `.length` sur un objet String Java utilise une approche différente pour trouver la longueur d'une chaîne de caractères, en utilisant le mécanisme de reflection Java. Cette méthode peut être plus efficace pour les chaînes de caractères plus longues, mais elle n'est pas spécifique à Clojure et n'utilise pas les fonctionnalités propres du langage.

# Voir aussi

- [Documentation officielle de la fonction `count`](https://clojuredocs.org/clojure.core/count)
- [Documentation officielle du protocole `Counted`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.protocols/Counted)
- [Guide de référence des opérateurs en Clojure](https://clojure.org/reference/protocols#counted)