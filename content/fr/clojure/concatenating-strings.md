---
title:    "Clojure: Concaténation de chaînes"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation des chaînes de caractères est une opération fréquemment utilisée en programmation pour combiner plusieurs chaînes en une seule. Cela peut être utile pour créer des messages personnalisés, des chaînes de requêtes ou tout simplement pour afficher des informations à l'utilisateur.

## Comment faire

```Clojure
(def s1 "Bonjour")
(def s2 "mon") 
(def s3 "ami")
(defn concat [s1 s2 s3] 
       (str s1 " " s2 " " s3 "!")) 
```

L'exemple ci-dessus déclare trois chaînes de caractères en utilisant `def`. Ensuite, la fonction `concat` prend ces chaînes et les combine en une seule en utilisant la fonction `str`. La chaîne finale sera "Bonjour mon ami!".

## Approfondissement

La concaténation des chaînes de caractères peut être effectuée de plusieurs façons en Clojure. Vous pouvez utiliser l'opérateur `+` pour concaténer deux chaînes ou utiliser la fonction `clojure.string/join` pour concaténer plusieurs chaînes avec un séparateur.

Il est également important de noter que Clojure utilise l'interface `java.lang.String` pour les chaînes de caractères. Cela signifie que vous pouvez utiliser toutes les méthodes de cette interface pour manipuler les chaînes, telles que `toLowerCase`, `toUpperCase` ou `substring`.

## Voir aussi

- [Documentation sur la concaténation de chaînes en Clojure](https://clojure.org/reference/strings)
- [Exemples pratiques de concaténation de chaînes en Clojure](https://www.baeldung.com/clojure-string-concatenation)
- [Autres opérations sur les chaînes en Clojure](https://www.sitepoint.com/clojure-strings-operations)