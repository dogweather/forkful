---
title:    "Clojure: Lecture des arguments de ligne de commande"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de lignes de commande dans la programmation peut sembler intimidante au premier abord, mais c'est en fait un outil très utile pour les développeurs. Apprendre à lire les arguments de ligne de commande peut grandement aider à personnaliser et à améliorer les programmes que vous créez en leur donnant une flexibilité supplémentaire.

## Comment Faire

Pour lire les arguments de ligne de commande en Clojure, vous pouvez utiliser la fonction "command-line-args". Elle prend en argument la ligne de commande entière, y compris le nom du programme. Voici un exemple de code avec un input et son output :

```Clojure
(def cmd (command-line-args))

;; Input : le programme est appelé avec les arguments "hello world"
(print "Bonjour" (first cmd) (last cmd))

;; Output : Bonjour hello world
```

Vous pouvez également utiliser la bibliothèque "clojure.tools.cli" pour une meilleure gestion des arguments. Voici un exemple avec cette bibliothèque :

```Clojure
(def opts [["-n" "--name NAME" "Votre nom"] ] ;; options possibles
(def prg {:name "Bonjour" :summary "Programme de salutation" :opts opts}) ;; définir le programme avec ses différentes options
(parse-opts (rest cmd) opts) ;; parse les options passées en paramètre
(if (name params)
    (println "Bonjour" (:name params))
    (println "Bonjour le monde"))
)

;; Input : le programme est appelé avec les arguments "-n John"
;; Output : Bonjour John
```

## Profondeur d'Analyse

En creusant un peu plus, vous pourrez découvrir différentes bibliothèques et outils qui vous permettront de gérer les arguments de ligne de commande avec encore plus de précision et de flexibilité. Par exemple, la bibliothèque "cli"-utils propose des fonctions pour lire les arguments de ligne de commande en utilisant des drapeaux (-n au lieu de --name) et pour gérer les valeurs par défaut pour les options non fournies.

Vous pouvez également vous pencher sur la gestion des options multiples, les validations d'options et la gestion des erreurs liées aux arguments de ligne de commande. Avec un peu de pratique et l'utilisation de ces outils, vous serez en mesure de gérer les arguments de ligne de commande en toute confiance.

## Voir Aussi

  - [Bibliothèque cli-utils] (https://github.com/ssanj/cli-utils)
  - [Bibliothèque clojure.tools.cli] (https://github.com/clojure/tools.cli)
  - [Article sur la lecture des arguments de ligne de commande avec Clojure] (https://purelyfunctional.tv/article/how-to-read-command-line-arguments-in-clojure/)