---
title:                "Clojure: Création d'un fichier temporaire"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est un moyen utile de stocker temporairement des données dans un programme Clojure. Cela peut être particulièrement pratique lorsqu'on travaille avec des données volumineuses ou en cas de besoin de sauvegarder des données intermédiaires.

## Comment faire

Pour créer un fichier temporaire en Clojure, nous allons utiliser la fonction `with-open` et la macro `clojure.java.io/file`. Cela nous permet de créer rapidement et proprement un fichier temporaire qui sera automatiquement supprimé à la fin du programme.

```Clojure
(with-open [temp-file (clojure.java.io/file "monFichierTemp")]
  (println "Le fichier temporaire est créé dans le répertoire:" (.getAbsolutePath temp-file)))
```

Lors de l'exécution de ce code, nous devrions voir l'emplacement du fichier temporaire imprimé dans la console. Ensuite, il suffit d'ajouter les données que nous voulons stocker dans le fichier en utilisant les fonctions de manipulation de fichiers de Clojure, par exemple `spit` ou `slurp`.

## Plongée en profondeur

Lors de la création d'un fichier temporaire en Clojure, il est important de noter que le fichier ne sera pas supprimé automatiquement si l'exécution du programme est interrompue avant la fin du bloc `with-open`. Pour éviter cela, nous pouvons utiliser la macro `with-exception-handling` pour attraper les exceptions et supprimer le fichier temporaire en cas d'erreur.

```Clojure
(with-exception-handling
  (with-open [temp-file (clojure.java.io/file "monFichierTemp")]
    (println "Le fichier temporaire est créé dans le répertoire:" (.getAbsolutePath temp-file))))
  (catch Exception e
    (when (.exists temp-file)
      (.delete temp-file))
    (throw e)))
```

Cette approche garantit que le fichier temporaire sera supprimé quelle que soit l'exception qui se produit.

## Voir aussi

Voici quelques ressources supplémentaires pour en savoir plus sur la création de fichiers temporaires en Clojure :

- [Documentation officielle](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Article de blog sur la manipulation de fichiers en Clojure](https://purelyfunctional.tv/guide/file-manipulation-in-clojure/)
- [Exemple de projet GitHub utilisant des fichiers temporaires en Clojure](https://github.com/knsv/mermaid/tree/master/src-clj)

En utilisant ces ressources, vous devriez être en mesure de créer facilement des fichiers temporaires dans vos programmes Clojure. Bon codage !