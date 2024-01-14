---
title:                "Clojure: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture des arguments en ligne de commande est une compétence essentielle pour tout développeur souhaitant avoir une compréhension approfondie de Clojure. Cela permet également de créer des applications plus dynamiques et flexibles en permettant aux utilisateurs de spécifier des options lors de l'exécution du programme.

## Comment faire

Pour lire les arguments en ligne de commande en Clojure, nous pouvons utiliser la fonction `command-line-args` qui renvoie une liste des arguments fournis lors de l'exécution du programme.

```Clojure
(defn -main
  "Fonction principale pour lire les arguments en ligne de commande"
  [& args]
  (let [command-line-args (take-last (count args) args))] ; Récupère les arguments après les options du programme
```

Par exemple, supposons que nous ayons un programme appelé `calculatrice.clj` et que nous souhaitons passer deux nombres en arguments pour effectuer des opérations. Nous pourrions utiliser la commande suivante: 

`clj calculatrice.clj 5 10`

Cela renverrait `[5 10]` en tant que résultat dans notre fonction `command-line-args`, que nous pourrions ensuite utiliser pour nos opérations.

## Plongée en profondeur

Il est important de noter que les arguments en ligne de commande sont automatiquement convertis en chaînes de caractères en Clojure. Cela signifie que si nous voulons utiliser des nombres ou des listes, nous devrons les convertir manuellement avec les fonctions `Integer/parseInt` et `clojure.string/split`.

De plus, il existe des bibliothèques telles que `clj-args` qui facilitent la lecture et la manipulation des arguments en ligne de commande en ajoutant des fonctionnalités telles que la validation des arguments et la gestion des options. 

## Voir aussi

- Documentation officielle Clojure sur la lecture des arguments en ligne de commande: [https://clojure.org/reference/programs#_command_line_arguments](https://clojure.org/reference/programs#_command_line_arguments)
- Tutoriel sur l'utilisation de la fonction `command-line-args`: [https://clojure.org/guides/learning/cljargs](https://clojure.org/guides/learning/cljargs)
- Bibliothèque `clj-args` pour une manipulation avancée des arguments en ligne de commande: [https://github.com/xsc/clj-args](https://github.com/xsc/clj-args)