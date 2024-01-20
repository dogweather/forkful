---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lire les Arguments de la Ligne de Commande en Clojure

## Pourquoi et Quoi ?

Lire les arguments de la ligne de commande en Clojure, c'est obtenir les options et paramètres fournis à votre programme lors de son lancement via le terminal. C'est une tâche commune en programmation pour personnaliser le comportement du programme selon les besoins de l'utilisateur.

## Comment faire :

Voici un exemple basique d'un programme qui lit et imprime les arguments de la ligne de commande.

```Clojure
(defn -main
  [& args]
  (println "Arguments: " args))
```

Si vous lancez le programme avec des arguments tels que "foo" et "bar", voici la sortie.

```Clojure
Arguments : [foo bar]
```

## Plongée en profondeur: 

Historiquement, les arguments de ligne de commande étaient le moyen principal pour les utilisateurs d'interagir avec les programmes. Bien que les interfaces utilisateur graphiques soient plus populaires aujourd'hui, les arguments de ligne de commande restent essentiels pour les applications sans interface graphique, les scripts et l'automatisation.

En alternative, on peut lire les entrées utilisateur avec les fonctions `read-line` ou `read`, ou lire des fichiers de configuration. Chacune a ses propres avantages et inconvénients.

Le système ‘parse-opts’ de Clojure vous permet de décortiquer facilement les arguments en options attendues et arguments positionnels, rendant la gestion des arguments de ligne de commande plus conviviale et moins sujette à erreurs.

## Voir Aussi:

- Documentation officielle Clojure sur les espaces de noms, qui explique comment les arguments de la ligne de commande sont passés à la fonction `-main`: [https://clojure.org/reference/namespaces#_the_namespace](https://clojure.org/reference/namespaces#_the_namespace)