---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:30.096080-07:00
description: "\xC9crire un fichier texte en Clojure implique de cr\xE9er ou de modifier\
  \ des fichiers pour sauvegarder des donn\xE9es en dehors de votre application, permettant\u2026"
lastmod: '2024-03-13T22:44:57.301328-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte en Clojure implique de cr\xE9er ou de modifier\
  \ des fichiers pour sauvegarder des donn\xE9es en dehors de votre application, permettant\u2026"
title: "R\xE9diger un fichier texte"
weight: 24
---

## Quoi & Pourquoi ?

Écrire un fichier texte en Clojure implique de créer ou de modifier des fichiers pour sauvegarder des données en dehors de votre application, permettant la persistance, la configuration, la journalisation ou la communication inter-processus. Les programmeurs effectuent cette tâche pour externaliser l'état de l'application, les configurations ou partager des informations entre différentes parties d'un programme ou différents programmes au total.

## Comment faire :

### Écrire du texte dans un fichier en utilisant les fonctions intégrées de Clojure

La fonction `spit` est le moyen le plus simple d'écrire du texte dans un fichier en Clojure. Elle prend deux arguments : le chemin du fichier et la chaîne à écrire. Si le fichier n'existe pas, `spit` le créera. S'il existe, `spit` l'écrasera.

```clojure
(spit "exemple.txt" "Bonjour, monde !")
```

Pour ajouter du texte à un fichier existant, vous pouvez utiliser la fonction `spit` avec l'option `:append`.

```clojure
(spit "exemple.txt" "\nAjoutons cette nouvelle ligne." :append true)
```

Après l'exécution de ces extraits, "exemple.txt" contiendra :

```
Bonjour, monde !
Ajoutons cette nouvelle ligne.
```

### Utiliser des bibliothèques tierces

Bien que les capacités intégrées de Clojure soient souvent suffisantes, la communauté a développé des bibliothèques robustes pour des tâches plus complexes ou spécifiques. Pour les entrées/sorties de fichiers, une bibliothèque populaire est `clojure.java.io`, qui propose une approche plus proche de Java pour la gestion des fichiers.

Pour utiliser `clojure.java.io` pour écrire dans un fichier, vous devez d'abord l'importer :

```clojure
(require '[clojure.java.io :as io])
```

Ensuite, vous pouvez utiliser la fonction `writer` pour obtenir un objet writer, et la fonction `spit` (ou d'autres comme `print`, `println`) pour écrire dans le fichier :

```clojure
(with-open [w (io/writer "exemple_avec_io.txt")]
  (.write w "Ceci est écrit en utilisant clojure.java.io"))
```

Cela créera (ou écrasera s'il existe déjà) "exemple_avec_io.txt" avec le texte :

```
Ceci est écrit en utilisant clojure.java.io
```

Souvenez-vous : `with-open` garantit que le fichier est correctement fermé après l'écriture, évitant les fuites de ressources potentielles.
