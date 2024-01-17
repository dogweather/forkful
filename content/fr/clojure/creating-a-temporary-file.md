---
title:                "Création d'un fichier temporaire"
html_title:           "Clojure: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi?

Créer un fichier temporaire en Clojure, c'est créer un fichier qui existe seulement pendant l'exécution du programme et qui est automatiquement supprimé après son utilisation. Les programmeurs créent des fichiers temporaires pour stocker des données de manière temporaire sans encombrer l'espace de stockage permanent.

# Comment faire:

Voici comment créer un fichier temporaire en Clojure en utilisant la fonction `with-open` :

```Clojure
(with-open [f (java.io.File/createTempFile "temp" ".txt")]
  (println "Fichier temporaire créé :" (.getPath f)))
```

La sortie sera quelque chose comme :

```
Fichier temporaire créé : /var/folders/_7/c79cjys51716015j7_xhszr00000gn/T/temp4878284577772479122.txt
```

# Plongée en profondeur:

## Contexte historique:
Créer des fichiers temporaires est une pratique courante en programmation, présente dans la plupart des langages de programmation. Cela remonte aux débuts de la programmation informatique, lorsque les ressources étaient limitées et qu'il fallait optimiser leur utilisation.

## Alternatives:
Outre la fonction `createTempFile` utilisée dans l'exemple précédent, il existe d'autres options pour créer des fichiers temporaires en Clojure, telles que `with-open-str` et `with-open-file`. Chacune a ses avantages et il est recommandé de choisir celle qui correspond le mieux à vos besoins.

## Détails de mise en oeuvre:
La fonction `createTempFile` utilise la classe `java.io.File` de Java pour créer un fichier temporaire. Elle prend en paramètre un préfixe et une extension pour nommer le fichier temporaire. Elle retourne ensuite un objet de type `File` qui peut être utilisé pour effectuer des opérations de lecture/écriture sur le fichier.

# Voir aussi:

- [Documentation de la fonction `createTempFile`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Plus d'informations sur la classe `java.io.File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)