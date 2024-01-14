---
title:    "Clojure: Vérifier si un répertoire existe"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de vérifier si un répertoire existe avant d'effectuer des opérations de création ou de suppression de fichiers pour éviter toute erreur ou conflit potentiel. Cela peut également aider à la gestion et l'organisation des fichiers dans un projet ou un système.

## Comment Faire

Pour vérifier si un répertoire existe en utilisant Clojure, nous pouvons utiliser la fonction `file-seq`, qui renvoie une séquence de fichiers et de sous-répertoires dans un répertoire donné. Ensuite, nous pouvons utiliser la fonction `some` pour itérer à travers cette séquence et vérifier si le répertoire que nous recherchons existe.

```Clojure
;; Vérifier si un répertoire existe 
(defn check-dir-exist [dir]
    (some #(= (:name %) dir) (file-seq (file "."))))
  
;; Exemple d'utilisation
(check-dir-exist "documents")
;; => true
(check-dir-exist "images")
;; => false

```

## Deep Dive

Il est important de noter que la méthode utilisée ci-dessus n'est pas la seule façon de vérifier si un répertoire existe en Clojure. Il existe également d'autres fonctions qui peuvent être utilisées, telles que `file?`, qui vérifie si un fichier ou un répertoire existe à un chemin spécifique, et `files`, qui renvoie une liste de tous les fichiers et répertoires dans un répertoire donné.

Il est également important de prendre en compte les différentes exceptions qui peuvent survenir lors de la vérification d'un répertoire, telles que les permissions insuffisantes ou les chemins invalides. Il est recommandé de gérer ces exceptions de manière appropriée afin de garantir le bon fonctionnement de votre code.

## Voir aussi

- [Documentation officielle de Clojure sur les répertoires et les fichiers](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Guide du débutant en Clojure](https://www.clojure.org/guides/getting_started)
- [Gestion des exceptions en Clojure](https://www.braveclojure.com/exceptions/)