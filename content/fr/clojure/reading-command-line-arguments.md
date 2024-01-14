---
title:    "Clojure: Lecture des arguments de ligne de commande"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez des programmes en Clojure, il est souvent utile de pouvoir lire des arguments de ligne de commande pour personnaliser l'exécution de votre code. Cela peut être particulièrement utile si vous souhaitez exécuter votre programme plusieurs fois avec des paramètres différents.

## Comment Faire

Pour lire des arguments de ligne de commande en Clojure, vous pouvez utiliser la fonction `command-line-args` de la bibliothèque standard `clojure.tools.cli`. Cette fonction renvoie une liste contenant les arguments fournis lors de l'exécution du programme. 

```Clojure
(ns mon-programme
  (:require [clojure.tools.cli :refer [command-line-args]]))

(def args (command-line-args))

(println "Les arguments fournis sont:" args)
```

Lors de l'exécution de ce programme avec la commande `clojure mon-programme.clj arg1 arg2`, vous obtiendrez en sortie: 

```
Les arguments fournis sont: [arg1 arg2]
```

## Plongée en Profondeur

`command-line-args` peut également prendre en charge des options de ligne de commande, telles que `-h` pour afficher l'aide ou `-v` pour activer le mode verbeux. Vous pouvez également définir des descriptions d'options et des valeurs par défaut en utilisant la fonction `parse-opts` de `clojure.tools.cli`. 

Pour une documentation complète sur comment utiliser `clojure.tools.cli`, consultez la [documentation officielle](https://github.com/clojure/tools.cli) et les [exemples de code](https://github.com/clojure/tools.cli/tree/master/examples).

## Voir Aussi

- [Documentation officielle de clojure.tools.cli](https://github.com/clojure/tools.cli) 
- [Exemples de code de clojure.tools.cli](https://github.com/clojure/tools.cli/tree/master/examples) 
- [Guide officiel pour apprendre Clojure](https://clojure.org/guides/learn/syntax)