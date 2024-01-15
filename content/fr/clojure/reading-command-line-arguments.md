---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Clojure: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez déjà utilisé des programmes en ligne de commande et vous vous demandez comment ils peuvent recevoir des arguments ? Cet article vous explique comment lire les arguments de la ligne de commande en utilisant Clojure.

## Comment faire

```Clojure
(def args *command-line-args*)
(print "Les arguments de la ligne de commande sont:")
(doseq [arg args] (println arg))
```

Pour lire les arguments de la ligne de commande, nous pouvons utiliser la variable spéciale `*command-line-args*` qui stocke tous les arguments passés lors de l'exécution du programme. En utilisant la fonction `doseq`, nous pouvons itérer à travers ces arguments et les imprimer à l'aide de `println`.

Voici un exemple d'exécution du programme avec des arguments :

```
$ clojure mon_programme.clj arg1 arg2 arg3
Les arguments de la ligne de commande sont:
arg1
arg2
arg3
```

## Plongée en profondeur

Il est également possible de spécifier des options et des valeurs pour les arguments de la ligne de commande. Par exemple, nous pouvons utiliser la librairie [`clojure.tools.cli`](https://github.com/clojure/tools.cli) pour traiter les arguments de manière plus structurée. Voici un exemple basique :

```Clojure
(ns mon-programme
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def cli-options [["-n" "--name" "Votre nom" :required true]])

(defn -main
  "Mon programme"
  [& args]
  (let [[opts params (parse-opts args cli-options)]]
	(if (:name opts)
      (println (str "Bonjour " (:name opts)))
      (println "Bonjour tout le monde!"))))
```

Là, nous spécifions que notre programme peut être appelé avec une option `-n` ou `--name` qui prendra une valeur en tant que notre nom. Nous utilisons `parse-opts` pour traiter les arguments et stocker leurs options et valeurs correspondantes dans la variable `opts`. Ensuite, nous pouvons utiliser ces valeurs dans notre programme, par exemple pour saluer le nom spécifié.

```
$ clojure mon_programme.clj -n Tom
Bonjour Tom
```

## Voir aussi

- [Documentation officielle de Clojure](https://clojure.org/reference/command_line_tools)
- [Librairie cli-tools pour gérer les arguments en ligne de commande](https://github.com/clojure/tools.cli)
- [Exemples avancés pour traiter les arguments de la ligne de commande en Clojure](https://github.com/jafingerhut/examples.cliargs)