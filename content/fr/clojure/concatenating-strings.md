---
title:                "Clojure: Concaténer des chaînes"
simple_title:         "Concaténer des chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est un aspect important de la programmation Clojure car elle permet de combiner plusieurs chaînes en une seule. Cela peut être utile pour créer des messages d'erreur, des noms de fichiers dynamiques ou même construire des requêtes de base de données.

## Comment faire

La concaténation de chaînes peut être réalisée en utilisant l'opérateur `str` ou la fonction `str`. Voici un exemple de code avec leur utilisation :

```Clojure
(str "Bonjour " "mon ami" "!") ;; Output: Bonjour mon ami!
(str "1 + 1 = " (+ 1 1)) ;; Output: 1 + 1 = 2
```

Dans cet exemple, nous combinons deux chaînes statiques avec une expression dynamique en utilisant `str`. Remarquez comment la valeur de l'expression est convertie en chaîne avant d'être concaténée.

Nous pouvons également utiliser `str` avec des variables pour concaténer des valeurs stockées :

```Clojure
(def nom "Marie")
(def age 25)
(str nom " a " age " ans.") ;; Output: Marie a 25 ans.
```

Si nous voulons concaténer plus de deux valeurs, nous pouvons utiliser la fonction `str` avec une liste de paramètres :

```Clojure
(str "La " "nuit " "dernière " "j'ai " "rêvé " "d'un " "serpent.")
```

## Plongeon Profond

En utilisant l'opérateur `str`, la concaténation de chaînes est réalisée en utilisant une méthode Java sous-jacente appelée `StringBuilder`. Cela signifie que `str` est optimisé pour les performances, en comparaison avec la fonction `str` qui utilise la méthode `StringBuilder` à chaque appel.

Il est également important de noter que la concaténation de chaînes peut avoir un impact sur le temps d'exécution si elle est utilisée fréquemment avec de grandes chaînes. Il est préférable d'utiliser la fonction `str` avec une liste de paramètres dans ce cas.

Enfin, en utilisant `str` ou la fonction `str` avec des expressions complexes ou une longue chaîne de concaténation peut être difficile à lire et à déboguer. Dans ces cas, il est recommandé d'utiliser la fonction `format` qui utilise la syntaxe de chaîne de format de Java.

## Voir aussi

- [Documentation officielle de Clojure sur la concaténation de chaînes](https://clojure.org/guides/faq#concatenation)
- [Tutoriel vidéo pour les débutants sur la concaténation de chaînes en Clojure](https://www.youtube.com/watch?v=RlYfFVK2_zA)
- [Un guide de bonnes pratiques pour la concaténation de chaînes en Java](https://www.baeldung.com/java-string-concatenation-performance)