---
title:                "Clojure: Lecture des arguments de ligne de commande"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous écrivez des programmes en Clojure, il peut arriver que vous ayez besoin d'interagir avec l'utilisateur au moment de l'exécution plutôt que de simplement exécuter un script sans aucune entrée. C'est là que la lecture des arguments de ligne de commande entre en jeu. Cela permet à l'utilisateur de fournir des informations spécifiques afin que le programme puisse les utiliser dans son exécution.

# Comment faire

Pour lire les arguments de ligne de commande en Clojure, vous pouvez utiliser la fonction `command-line-args` qui renvoie une liste des arguments passés lors de l'exécution du programme. Par exemple, si vous souhaitez passer un nom d'utilisateur et un mot de passe lors de l'exécution du programme, vous pouvez le faire de la manière suivante :

```Clojure
(def username (nth (command-line-args) 0))
(def password (nth (command-line-args) 1))

(println "Bienvenue" username "! Votre mot de passe est" password)
```

Lorsque vous exécutez ce programme et lui passez les arguments "Alice" et "1234", la sortie sera la suivante :

```
Bienvenue Alice ! Votre mot de passe est 1234
```

Vous pouvez également utiliser la fonction `count` pour vérifier le nombre d'arguments passés afin d'éviter les erreurs d'index hors limites.

# Plongée en profondeur

Il est également possible d'utiliser une bibliothèque externe telle que `clojure.tools.cli` pour gérer les arguments de ligne de commande de manière plus avancée. Cette bibliothèque vous permet de définir des options avec des noms, des raccourcis et des descriptions, ainsi que de valider les arguments pour s'assurer qu'ils sont correctement formatés.

Par exemple, si nous créons une option pour un argument `--age` qui doit être un nombre entier, nous pouvons l'utiliser comme ceci :

```Clojure
(def options
  [["-a" "--age" "Âge de l'utilisateur" :validate #(and (number? (Integer/parseInt %)) (pos? (Integer/parseInt %))))]
(def parsed-options (cli/parse-opts (command-line-args) options))

(def age (:age parsed-options)) 
(println "L'utilisateur a" age "ans")
```

Lorsque vous exécutez ce programme en passant un âge incorrect tel que "vingt" au lieu d'un nombre, il renverra une erreur car l'âge doit être un nombre entier positif.

# Voir aussi

- Référence officielle : https://clojure.org/reference/commandline
- Tutoriel sur la lecture des arguments de ligne de commande : https://purelyfunctional.tv/guide/command-line-intro/
- Documentation de la bibliothèque `clojure.tools.cli` : https://github.com/clojure/tools.cli