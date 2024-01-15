---
title:                "Lecture d'un fichier texte"
html_title:           "Clojure: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation en Clojure, vous avez probablement déjà entendu parler de la fonctionnalité de lecture de fichiers textes. Mais pourquoi voudriez-vous apprendre à le faire ? Tout d'abord, cela vous permettra de manipuler et d'analyser des données provenant de sources externes, ce qui est souvent nécessaire dans le développement de logiciels. De plus, comprendre comment lire des fichiers textes en Clojure vous permettra de mieux comprendre la manipulation de chaînes de caractères et les fonctions de manipulation de fichiers.

## Comment faire

Pour lire un fichier texte en Clojure, vous allez utiliser la fonction "slurp". Voici un exemple de code pour lire le contenu d'un fichier texte :

```Clojure
(slurp "mon_fichier.txt")
```

Ce code renverra une chaîne de caractères contenant tout le contenu du fichier. Si vous voulez stocker ce contenu dans une variable, vous pouvez utiliser le "let binding" comme ceci :

```Clojure
(let [mon_fichier (slurp "mon_fichier.txt")]
  (println mon_fichier))
```

Dans cet exemple, "mon_fichier" sera une variable contenant le contenu du fichier texte et la fonction "println" l'imprimera dans la console.

## Plongée en profondeur

La fonction "slurp" peut également être utilisée pour lire des fichiers en ligne en passant l'URL du fichier en tant que paramètre. De plus, il existe d'autres fonctions de manipulation de fichiers en Clojure telles que "with-open" qui gère automatiquement la fermeture du fichier une fois que vous avez fini de le lire, ou "line-seq" qui lit un fichier ligne par ligne.

Il est également important de noter que la fonction "slurp" renvoie une chaîne de caractères brute sans la diviser en lignes. Pour cela, vous pouvez utiliser la fonction "clojure.string/split-lines" pour diviser la chaîne en fonction des retours à la ligne.

## Voir aussi

- Documentation officielle pour la fonction "slurp" : https://clojuredocs.org/clojure.core/slurp
- Tutoriel sur la manipulation de fichiers avec Clojure : https://clojure.org/guides/io
- Exemples de code pour lire et écrire des fichiers en Clojure : https