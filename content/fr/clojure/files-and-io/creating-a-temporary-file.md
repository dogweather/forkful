---
date: 2024-01-20 17:39:59.029938-07:00
description: "Cr\xE9er un fichier temporaire, c'est produire un fichier destin\xE9\
  \ \xE0 une utilisation courte et unique. Les programmeurs le font pour stocker des\
  \ donn\xE9es\u2026"
lastmod: '2024-03-13T22:44:57.302370-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire, c'est produire un fichier destin\xE9 \xE0\
  \ une utilisation courte et unique."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## How to:
En Clojure, on utilise la librairie Java intégrée pour créer et manipuler des fichiers temporaires. Voici comment faire:

```Clojure
(import '(java.io File)
        '(java.nio.file Files))

(defn create-temp-file [prefix suffix]
  (.toFile (Files/createTempFile prefix suffix)))

(def temp-file (create-temp-file "example" ".tmp"))
(println "Temporary file created:" temp-file)
```

Sortie de l'exemple :
```
Temporary file created: /tmp/example1234567890.tmp
```

## Deep Dive
Historiquement, les fichiers temporaires sont sur scène depuis l'ère des premiers systèmes d'exploitation pour séparer les données persistantes des temporaires. En Clojure, on s'appuie sur les capacités de Java, vu que Clojure est hébergé sur la JVM. Une alternative est d'utiliser des bibliothèques tierces, mais Java fournit déjà ce dont on a besoin efficacement.

L'implémentation détaillée implique la gestion sécuritaire de ces fichiers, car ils peuvent introduire des vulnérabilités de sécurité s'ils ne sont pas manipulés correctement, comme par exemple en évitant les conflits de noms et en assurant la suppression après usage.

## See Also
Pour aller plus loin, consultez ces ressources:

- JavaDoc pour [java.nio.file.Files](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
- Guide Clojure sur l'interopérabilité Java: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
- Sécurité des fichiers temporaires: [https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File](https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File)
