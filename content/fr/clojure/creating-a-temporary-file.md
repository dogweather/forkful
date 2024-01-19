---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?
La création de fichiers temporaires est un processus dans lequel un programme crée un fichier qui est utilisé pendant une durée limitée. Les programmeurs le font surtout pour gérer une grande quantité de données pendant l'exécution d'un programme, pour des besoins spécifiques de traitement de données ou pour du débogage.

## Comment faire :
Créons un fichier temporaire dans Clojure. On peut utiliser la fonction `java.io.File/createTempFile`.

```Clojure
(import 'java.io.File)

(defn create-temp-file [prefix suffix]
  (.createTempFile File prefix suffix))

(println (create-temp-file "temp" ".txt"))
```

Cela va créer un fichier temporaire avec le préfixe "`temp`" et l'extension "`txt`" et va imprimer le chemin vers le fichier.

## Plongée en Profondeur
- Historiquement, la gestion des fichiers temporaires est intrinsèque à l'informatique depuis ses débuts. 
- En ce qui concerne les alternatives, différentes plateformes et langages de programmation offrent des solutions pour créer et gérer des fichiers temporaires. Par exemple, en Java, on peut utiliser la classe `java.nio.file.Files` avec sa méthode `createTempFile()`.
- Dans le contexte de Clojure - un langage sur la JVM - on a directement accès à la méthode `createTempFile` de la classe java.io.File. Le fichier créé sera supprimé lors de l'arrêt de la JVM si on utilise la méthode `deleteOnExit()` sur l'instance du fichier.

## Voir Aussi
- La documentation sur [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)).
- Un guide sur la gestion des fichiers temporaires sur différentes [plateformes](https://www.tutorialspoint.com/java/io/file_createtempfile_directory.htm).
- Une discussion StackOverflow sur les fichiers temporaires en [Clojure](https://stackoverflow.com/questions/29410479/create-a-temporary-file-in-clojure).