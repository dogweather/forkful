---
title:                "Vérifier l'existence d'un répertoire"
html_title:           "Clojure: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Vérifier si un répertoire existe est simplement la vérification pour savoir si un répertoire donné est présent sur votre système de fichiers. Les programmeurs effectuent souvent cette vérification pour s'assurer que les fichiers peuvent être correctement lus ou écrits dans un emplacement donné.

## Comment faire :
Voici comment vérifier si un répertoire existe en utilisant Clojure :

```
(if (java.io.File. "/chemin/vers/repertoire").isDirectory())
  (println "Le répertoire existe !"))
```

Le code ci-dessus crée un objet `java.io.File` pour le chemin donné puis utilise la méthode `isDirectory()` pour vérifier s'il s'agit d'un répertoire. Si oui, il affiche un message indiquant que le répertoire existe.

## Plongée en profondeur :
Dans le passé, cette vérification était effectuée en utilisant la commande système `ls` pour lister les fichiers et répertoires dans un emplacement donné. Cependant, cela n'est pas une méthode fiable car cela dépend du système d'exploitation et des autorisations utilisateur. Aujourd'hui, la méthode la plus courante pour vérifier si un répertoire existe est d'utiliser la classe `java.io.File` comme présenté précédemment.

## Voir aussi :
Pour en savoir plus sur la manipulation de fichiers en Clojure, vous pouvez consulter la documentation officielle sur les entrées/sorties : https://clojure.org/reference/io. Vous pouvez également consulter la bibliothèque `clj-io` pour faciliter la manipulation de fichiers et répertoires en Clojure : https://github.com/clj-commons/clj-io.