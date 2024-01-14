---
title:    "Clojure: Création d'un fichier temporaire"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une technique utile dans la programmation Clojure. Cela permet de stocker temporairement des données avant de les traiter ou de les utiliser dans une autre partie du code.

## Comment faire

Pour créer un fichier temporaire en Clojure, utilisez la fonction `with-open`. Cela garantit que le fichier sera fermé automatiquement une fois que vous avez terminé de l'utiliser.

Voici un exemple de code montrant comment créer un fichier temporaire et y écrire du contenu :

```Clojure
(with-open [f (java.io.File/createTempFile "mon-fichier" nil)]
  (spit f "Contenu de mon fichier"))
```

Dans cet exemple, le fichier temporaire sera créé dans le répertoire par défaut pour les fichiers temporaires, sans préfixe et avec l'extension `.tmp`. Vous pouvez spécifier un préfixe et une extension en passant des chaînes en deuxième et troisième arguments respectivement.

Le contenu du fichier peut être écrit en utilisant la fonction `spit` de Clojure qui prend en premier argument le chemin du fichier et en deuxième argument le contenu à écrire.

## Plongée en profondeur

En utilisant la fonction `with-open`, nous pouvons également spécifier un comportement personnalisé pour la fermeture du fichier. Par exemple, vous pouvez définir une fonction à exécuter avant de fermer le fichier, comme ceci :

```Clojure
(with-open [f (java.io.File/createTempFile "mon-fichier" nil)]
  (spit f "Contenu de mon fichier")
  (println "Fichier temporaire créé à l'emplacement" (.getPath f)))

```

De plus, vous pouvez également définir un en-tête pour le fichier en utilisant la fonction `set-file-headers` de Clojure :

```Clojure
(with-open [f (java.io.File/createTempFile "mon-fichier" nil)]
  (set-file-headers f {:auteur "Jane Doe" :date "06/08/2021"})
  (spit f "Contenu de mon fichier")
  (println "Fichier temporaire créé avec l'en-tête" (.getHeader f)))
```

## Voir aussi

* [Documentation officielle de Clojure sur les fichiers temporaires](https://clojure.org/reference/java_interop#_create_temp_file)
* [Tutoriel sur la manipulation des fichiers en Clojure](https://www.baeldung.com/clojure-read-write-files)