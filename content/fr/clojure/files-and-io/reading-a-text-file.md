---
date: 2024-01-20 17:53:47.761549-07:00
description: "Lire un fichier texte, c'est extraire son contenu pour l'utiliser. Les\
  \ programmeurs le font pour traiter des donn\xE9es, configurer des programmes ou\u2026"
lastmod: '2024-03-13T22:44:57.300378-06:00'
model: gpt-4-1106-preview
summary: "Lire un fichier texte, c'est extraire son contenu pour l'utiliser. Les programmeurs\
  \ le font pour traiter des donn\xE9es, configurer des programmes ou\u2026"
title: Lecture d'un fichier texte
weight: 22
---

## What & Why?
Lire un fichier texte, c'est extraire son contenu pour l'utiliser. Les programmeurs le font pour traiter des données, configurer des programmes ou simplement importer des informations.

## How to:
Lire un fichier en Clojure est simple. Utilisez `slurp` pour un petit fichier :

```Clojure
(def contenu (slurp "chemin/vers/le/fichier.txt"))
(println contenu) ; Affiche le contenu du fichier
```

Si le fichier est volumineux, lisez-le ligne par ligne :

```Clojure
(with-open [r (reader "chemin/vers/le/fichier.txt")]
  (doseq [ligne (line-seq r)]
    (println ligne))) ; Affiche chaque ligne du fichier
```

## Deep Dive:
Historiquement, la lecture de fichiers est un élément fondamental de programmation – on stocke et on récupère des données.

En Clojure, `slurp` est bien pour les petits fichiers, mais gare à la mémoire ! Pour des fichiers plus gros, `line-seq` et `with-open` permettent une lecture économe.

Alternativement, pour plus de contrôle, on peut utiliser `java.io` directement (Clojure est sur la JVM, après tout !). Il y a aussi des bibliothèques comme `clojure.java.io` pour plus d'options et flexibilité.

## See Also:
- [ClojureDocs `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [ClojureDocs `with-open`](https://clojuredocs.org/clojure.core/with-open)
- [The Java IO Tutorial](https://docs.oracle.com/javase/tutorial/essential/io/) (Parce que Clojure tourne sur Java)
