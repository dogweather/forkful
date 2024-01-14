---
title:    "Clojure: Afficher la sortie de débogage"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Pourquoi

La sortie de débogage est un outil essentiel pour tout programmeur. Elle permet de suivre l'exécution du code et de détecter les erreurs. Dans cet article, nous allons voir comment utiliser la sortie de débogage en Clojure pour faciliter le processus de débogage.

## Comment faire

Pour imprimer la sortie de débogage en Clojure, vous pouvez utiliser la fonction `println`. Elle prend en paramètre une ou plusieurs valeurs à imprimer. Par exemple :

```clojure
(println "Hello World!")
(println "Le résultat de l'addition est:" (+ 5 6))
```

Cette fonction est particulièrement utile lorsque vous souhaitez suivre les valeurs de variables ou d'expressions à des points spécifiques de votre code. Elle peut également être combinée avec la fonction `str` pour imprimer des variables avec des chaînes de caractères pour plus de clarté :

```clojure
(def nom "Pierre")
(def age 20)
(println (str "Bonjour, je m'appelle " nom " et j'ai " age " ans."))
```

Ce qui affichera dans la console :

```
Bonjour, je m'appelle Pierre et j'ai 20 ans.
```

Vous pouvez également utiliser la fonction `format` pour imprimer les valeurs avec un format spécifique :

```clojure
(def nombre 100)
(println (format "Le nombre en binaire est %b" nombre))
```

Ce qui affichera :

```
Le nombre en binaire est 1100100
```

## Plongée profonde

Il est également possible d'utiliser la macro `prn` en lieu et place de la fonction `println`. La différence principale est que `prn` imprimera la sortie avec des guillemets et `println` sans. Par exemple :

```clojure
(prn "Hello World!")
```

affichera :

```
"Hello World!"
```

Alors que la même instruction avec `println` :

```clojure
(println "Hello World!")
```

affichera simplement :

```
Hello World!
```

Enfin, il existe également les macros `pprint` et `pretty` qui permettent un affichage plus lisible et mieux formaté des données complexes telles que les listes et les maps.

## Voir aussi

- [Documentation officielle de Clojure pour la sortie de débogage](https://clojure.org/guides/debugging)
- [Article sur la sortie de débogage en Clojure](https://purelyfunctional.tv/guide/functional-programming-tips/clojure-debugging/)