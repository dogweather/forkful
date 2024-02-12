---
title:                "Lecture des arguments de ligne de commande"
aliases:
- /fr/clojure/reading-command-line-arguments.md
date:                  2024-01-20T17:55:33.732185-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire les arguments de la ligne de commande, c'est récupérer des infos directement de l'utilisateur quand il exécute votre script. On fait ça pour rendre nos programmes flexibles et interactifs.

## How to:
En Clojure, on utilise la variable globale `*command-line-args*` pour accéder aux arguments. Regardez comment on fait :

```clojure
;; Lancer le programme : `clojure monscript.clj arg1 arg2`

(defn main []
  (println "Arguments de la ligne de commande:" *command-line-args*))

(main)
```

Sortie typique :

```
Arguments de la ligne de commande: (arg1 arg2)
```

On peut aussi parcourir les arguments :

```clojure
;; Lancer le programme : `clojure monscript.clj pomme orange banane`

(defn main []
  (doseq [arg *command-line-args*]
    (println "Fruit:" arg)))

(main)
```

Sortie :

```
Fruit: pomme
Fruit: orange
Fruit: banane
```

## Deep Dive
Avant, en Clojure, on utilisait `*command-line-args*` directement, mais c'est un peu brut. Aujourd'hui, on préfère souvent des bibliothèques comme `tools.cli` pour une analyse plus fine. 

`*command-line-args*` est un vestige des premiers jours de Clojure, mais c’est toujours là, simple et pur. Si besoin est, on utilise des packages spécialisés qui s'occupent des drapeaux et des options (comme `tools.cli`, qui est un choix solide).

Pourquoi pas utiliser `*command-line-args*` pour tout ? Parce que dès que les choses se corsent (options complexes, besoin de validation, etc.), tu vas vouloir quelque chose de plus costaud. 

## See Also
Pour approfondir, voici quelques liens :

- [Clojure.org sur les scripts](https://clojure.org/guides/deps_and_cli)
- [Documentation de tools.cli](https://github.com/clojure/tools.cli)
