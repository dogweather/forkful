---
title:    "Clojure: Lecture d'un fichier texte"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Lire des fichiers texte est une activité courante en programmation, que ce soit pour lire des données à traiter ou pour afficher du contenu dans une application. Dans cet article, nous allons explorer comment lire des fichiers texte en utilisant Clojure, un langage de programmation dynamique et fonctionnel basé sur Java.

## Comment faire

Pour lire un fichier texte avec Clojure, nous allons utiliser la fonction `slurp`. Cette fonction prend en paramètre le chemin vers le fichier à lire et renvoie une chaîne de caractères contenant tout son contenu. Voyons un exemple :

```Clojure
(def chemin "chemin/vers/mon/fichier/texte.txt")
(slurp chemin)
```

Cela va retourner le contenu du fichier texte sous forme de chaîne de caractères. Si nous voulons traiter chaque ligne du fichier, nous pouvons utiliser la fonction `line-seq` qui va diviser le contenu en lignes. Voici un exemple complet :

```Clojure
(def chemin "chemin/vers/mon/fichier/texte.txt")
(def lignes (line-seq (slurp chemin)))

;; afficher chaque ligne du fichier
(doseq [ligne lignes]
  (println ligne))
```

La fonction `doseq` va parcourir chaque ligne du fichier et utiliser la fonction `println` pour afficher son contenu dans la console. Nous pouvons également utiliser `clojure.string/split` pour diviser chaque ligne en différents éléments en fonction d'un séparateur. Voici un exemple :

```Clojure
(def chemin "chemin/vers/mon/fichier/texte.txt")
(def lignes (line-seq (slurp chemin)))

;; séparer chaque ligne par le caractère "|"
(doseq [ligne lignes]
  (println (clojure.string/split ligne #"\|")))
```

Nous pouvons ainsi traiter chaque ligne selon nos besoins en utilisant des fonctions de manipulation de chaînes de caractères comme `indexOf` ou `replace`. Une fois que nous avons fini de lire le fichier, il est important de fermer le flux en appelant la fonction `close` sur le fichier. Cela garantit que toutes les ressources sont correctement libérées.

## Plongée en profondeur

Maintenant que nous savons comment lire des fichiers texte en utilisant Clojure, voyons quelques autres astuces pour faciliter cette tâche. Tout d'abord, nous pouvons spécifier le charset (jeu de caractères) du fichier que nous lisons pour nous assurer que l'encodage est correct. Cela peut être fait en utilisant la fonction `with-open` :

```Clojure
(def chemin "chemin/vers/mon/fichier/texte.txt")

(with-open [flux (reader chemin :charset "UTF-8")]
  (doseq [ligne (line-seq flux)]
    (println ligne)))
```

De plus, pour les fichiers volumineux, il peut être plus efficace d'utiliser la fonction `with-open` avec la fonction `reader` plutôt que d'utiliser `slurp`, car cela permet de lire le fichier en utilisant un flux plutôt que de charger tout le contenu en mémoire.

Il est également important de gérer les erreurs lors de la lecture d'un fichier. Par exemple, si le chemin spécifié ne correspond à aucun fichier, une erreur sera renvoyée. Nous pouvons utiliser la fonction `try/catch` pour gérer ces erreurs et les afficher de manière plus appropriée à l'utilisateur.

## Voir aussi

- Documentation officielle de `slurp` : https://clojuredocs.org/clojure.core/slurp
- Tutoriel sur la lecture des fichiers en Clojure : https://www.baeldung.com/clojure-read-file
- Conseils pour gérer les erreurs lors de la lecture de fichiers : https://purelyfunctional.tv/article/handle-errors-in-files/

Merci d'avoir lu cet article sur la lecture des fichiers texte en Clojure. Nous espérons que cela vous a été utile et que vous vous sentez à l'aise pour utiliser ces fonctions dans vos propres projets. N'hésitez pas à explorer davantage et à expérimenter avec ces fonctionnalités pour mieux les comprendre. Bonne programmation !