---
title:                "Clojure: Vérification de l'existence d'un répertoire"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez avec des fichiers et des répertoires en programmation, il est important de vérifier si un répertoire existe avant d'effectuer des opérations dessus. Cela peut éviter des erreurs et faciliter la gestion des fichiers dans votre code.

## Comment faire

Pour vérifier si un répertoire existe en utilisant Clojure, vous pouvez utiliser la fonction `clojure.java.io/file`. Cette fonction prend en argument le chemin du répertoire et retourne un objet `java.io.File` qui représente le répertoire. Ensuite, vous pouvez utiliser la méthode `.exists` de cet objet pour déterminer si le répertoire existe ou non.

Voici un exemple de code pour vérifier si un répertoire existe :

```Clojure
(def directory (clojure.java.io/file "/chemin/vers/le/répertoire"))
(println (.exists directory)) ; affiche true si le répertoire existe, false sinon
```

## Plongée en profondeur

Lorsque vous utilisez la méthode `exists` pour vérifier l'existence d'un répertoire, il est important de comprendre le comportement de cette méthode. Selon la documentation officielle de Java, la méthode `.exists` peut renvoyer `false` si :

- Le répertoire n'existe pas
- Vous n'avez pas les permissions nécessaires pour accéder au répertoire
- Une erreur s'est produite lors de l'appel de la méthode

Par conséquent, il est important de mettre en place une gestion d'erreur adéquate pour gérer ces cas.

# Voir aussi

- Documentation officielle de Java sur la classe `java.io.File` : https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Documentation sur la fonction `clojure.java.io/file` : https://clojuredocs.org/clojure.java.io/file