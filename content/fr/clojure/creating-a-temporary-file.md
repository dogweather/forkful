---
title:                "Clojure: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi

Créer des fichiers temporaires est une tâche courante en programmation qui peut être utile pour stocker des données temporaires ou pour gérer des fichiers de manière dynamique. Cela peut également être utile lors du débogage de code, en permettant de manipuler des données sans affecter les données réelles.

## Comment faire

Il existe différentes façons de créer des fichiers temporaires en Clojure. L'une des méthodes les plus courantes consiste à utiliser la fonction `with-open` en combinaison avec la fonction `clojure.java.io/file`. Voici un exemple de code pour créer un fichier temporaire :

```Clojure
(with-open [file (clojure.java.io/file "mytempfile.txt")]
  (write-line! file "This is my temporary file!"))
```

Cela va créer un fichier appelé "mytempfile.txt" dans le répertoire courant et y écrire la ligne spécifiée.

## Plongée en profondeur

Pour comprendre comment la fonction `with-open` et la fonction `clojure.java.io/file` fonctionnent, il est important de comprendre comment Clojure gère les fichiers. En utilisant les fonctions `with-open` et `file`, Clojure va créer un objet `java.io.File` qui représente le fichier temporaire. Lorsque le code à l'intérieur de `with-open` est exécuté, ce fichier est ouvert et le code peut écrire ou lire à partir de celui-ci. Une fois la fin de `with-open` atteinte, le fichier sera automatiquement fermé et le fichier temporaire sera supprimé.

## Voir aussi

- [La documentation de Clojure sur les fichiers et les répertoires](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Un article sur les fichiers temporaires en Clojure](https://purelyfunctional.tv/guide/temporary-files-clojure/)