---
title:                "Clojure: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte est un moyen simple et efficace de stocker des informations dans un format lisible par l'humain. Cela peut être utile pour créer des rapports, des listes ou tout simplement pour noter des idées.

## Comment faire

Le langage de programmation Clojure offre une syntaxe rapide et efficace pour écrire des fichiers texte. Voici un exemple de code qui crée un fichier texte avec deux lignes contenant des nombres :

```Clojure
(with-open [f (clojure.java.io/writer "mon_fichier.txt")]
  (.write f "30\n")
  (.write f "15\n"))
```

En exécutant ce code, un fichier nommé "mon_fichier.txt" sera créé dans le même dossier que votre code Clojure. Son contenu sera :

```
30
15
```

Vous pouvez également intégrer des données dynamiques dans votre fichier en utilisant des variables et des boucles. Voici un exemple qui crée un fichier avec les noms de trois fruits :

```Clojure
(with-open [f (clojure.java.io/writer "fruits.txt")]
  (doseq [fruit ["pomme" "banane" "fraise"]]
    (.write f (str fruit "\n"))))
```

Le fichier "fruits.txt" contiendra :

```
pomme
banane
fraise
```

## Plongée en profondeur

En plus de créer des fichiers texte, Clojure offre également des fonctions utiles pour lire et manipuler des fichiers déjà existants. Par exemple, la fonction `slurp` permet de lire le contenu d'un fichier en une seule ligne de code :

```Clojure
(slurp "mon_fichier.txt")
```

Cela renverra le contenu du fichier sous forme de chaîne de caractères. Vous pouvez également utiliser des fonctions telles que `subs` pour extraire une partie spécifique du contenu ou `reduce` pour effectuer des opérations sur chaque ligne du fichier.

## Voir aussi

Pour en savoir plus sur la manipulation de fichiers en Clojure, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Clojure](https://clojuredocs.org/clojure.core/slurp)
- [Tutoriel sur la manipulation de fichiers avec Clojure](https://www.clojure.org/guides/io)
- [Exemples de code pour écrire, lire et manipuler des fichiers](https://clojuredocs.org/clojure.java.io)

Maintenant que vous avez appris à écrire des fichiers texte en utilisant Clojure, vous pouvez les utiliser pour stocker et organiser vos données de manière plus efficace. À vous de jouer !