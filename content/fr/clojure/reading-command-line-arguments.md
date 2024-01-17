---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Clojure: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi le font-ils ?
La lecture des arguments de la ligne de commande consiste à récupérer les informations saisies par l'utilisateur lorsqu'il exécute un programme en utilisant la ligne de commande. Les programmeurs le font pour pouvoir personnaliser le comportement de leur programme en fonction des arguments fournis par l'utilisateur.

Comment faire :
```Clojure
(defn read-args []
  (vec *command-line-args*))

(defn print-args [args]
  (println "Les arguments saisis sont :" args))

;; Exemple d'utilisation
(def args (read-args))
(print-args args)
```
Sortie :
```
Les arguments saisis sont : ["argument1" "argument2"]
```

Plongée en profondeur :
Dans le passé, les programmes étaient principalement exécutés en utilisant des interfaces utilisateur graphiques (GUI). Cependant, avec l'avènement des langages de script et des systèmes d'exploitation en ligne de commande, lire les arguments de la ligne de commande est devenu une pratique courante pour les programmeurs. Il existe également des alternatives telles que l'utilisation de fichiers de configuration ou l'interaction avec l'utilisateur via des menus.

Implémentation :
En Clojure, la fonction `*command-line-args*` retourne une séquence d'arguments saisis par l'utilisateur lors de l'exécution du programme en ligne de commande. La fonction `vec` est utilisée pour convertir la séquence en un vecteur pour faciliter la manipulation. La fonction `println` est utilisée pour afficher les arguments saisis par l'utilisateur.

Voir aussi :
Pour plus d'informations sur la lecture des arguments de la ligne de commande en Clojure, consultez la documentation officielle : https://clojure.org/reference/command_line_tools