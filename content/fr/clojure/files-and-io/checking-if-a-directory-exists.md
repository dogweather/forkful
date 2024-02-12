---
title:                "Vérifier si un répertoire existe"
aliases: - /fr/clojure/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:02.483152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe en Clojure consiste à confirmer la présence d'un répertoire dans le système de fichiers depuis votre application Clojure. Cette tâche est cruciale pour les opérations sur les fichiers, afin de prévenir les erreurs lors de la lecture ou de l'écriture dans des répertoires qui pourraient ne pas exister, garantissant ainsi une exécution de code robuste et sans erreur.

## Comment :
Clojure, étant un langage JVM, peut utiliser la classe `java.io.File` de Java à cette fin. Vous n'avez pas besoin de bibliothèque tierce pour une opération aussi basique. Voici comment vous pouvez procéder :

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Exemple d'utilisation
(println (directory-exists? "/path/to/your/directory")) ;; true or false
```

Cette fonction, `directory-exists?`, prend un chemin de répertoire sous forme de chaîne et retourne `true` si le répertoire existe et `false` dans le cas contraire. Cela est réalisé en créant un objet `File` avec le chemin du répertoire, puis en appelant la méthode `.exists` sur cet objet.

En plus de l'interopérabilité brute avec Java, vous pouvez utiliser des bibliothèques Clojure qui abstraient une partie du code standard Java. Une de ces bibliothèques est `clojure.java.io`. Cependant, pour vérifier si un répertoire existe, vous utiliserez toujours la classe `File`, mais vous pourriez trouver la bibliothèque utile pour d'autres opérations sur les fichiers. Exemple :

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Exemple d'utilisation
(println (directory-exists?-clojure "/another/path/to/check")) ;; true or false
```

Cette version est assez similaire mais utilise la fonction `io/file` de Clojure pour créer l'objet `File`. Cette méthode s'intègre plus naturellement dans les bases de code Clojure en tirant parti de la bibliothèque Clojure pour les opérations d'IO, plutôt qu'en interfaçant directement avec les classes Java.
