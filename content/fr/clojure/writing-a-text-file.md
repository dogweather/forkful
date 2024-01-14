---
title:    "Clojure: Écrire un fichier texte"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Clojure ?

L'écriture de fichiers texte en Clojure est une compétence utile à posséder en tant que programmeur. Cela vous permet de stocker et de manipuler des données de manière organisée et durable. Que vous souhaitiez enregistrer des données utilisateur ou créer un fichier de configuration pour votre application, la capacité d'écrire un fichier texte sera un atout précieux.

## Comment faire

Pour écrire un fichier texte en Clojure, il y a quelques étapes à suivre :

1. Importez la bibliothèque `clojure.java.io` en haut de votre fichier. Cela vous permettra d'utiliser des fonctions pour manipuler les fichiers.
2. Définissez le chemin et le nom de votre fichier à l'aide de la fonction `file` de la bibliothèque `clojure.java.io`.
3. Utilisez la fonction `with-open` pour créer et écrire dans le fichier. Cette fonction garantit que le fichier sera fermé correctement après utilisation.
4. Utilisez la fonction `spit` pour écrire du contenu dans le fichier, en utilisant le chemin et le nom du fichier définis précédemment.

Voici un exemple de code :

```Clojure
(ns mon.app
  (:require [clojure.java.io :as io]))

(defn ecrire-fichier [chemin contenu]
  (with-open [f (io/file chemin)]
    (spit f contenu)))

;; Exemple d'utilisation
(ecrire-fichier "monfichier.txt" "Ceci est un exemple de contenu.")
```

Lorsque vous exécutez ce code, un fichier nommé "monfichier.txt" sera créé et le contenu sera écrit dedans.

## Plongée en profondeur

En plus d'écrire du contenu dans un fichier, vous pouvez également utiliser des fonctions pour lire et modifier des fichiers existants. Par exemple, la fonction `slurp` permet de lire le contenu d'un fichier dans une chaîne de caractères, et la fonction `spit` permet de modifier le contenu d'un fichier existant.

Il est également possible de spécifier le format du fichier que vous souhaitez écrire. Par exemple, en utilisant la fonction `println` plutôt que `spit`, vous pouvez écrire du contenu dans un fichier au format CSV.

## Voir aussi

- Documentation de Clojure pour la bibliothèque `clojure.java.io` : https://clojure.github.io/clojure/clojure.java.io-api.html
- Tutoriel sur l'écriture de fichiers en Clojure : https://www.brainbell.com/tutors/Clojure/Writing_Files.htm
- Exemple de projet GitHub utilisant l'écriture de fichiers en Clojure : https://github.com/apa512/clj-password-manager