---
title:                "Création d'un fichier temporaire"
html_title:           "Java: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Créer un fichier temporaire en programmation signifie créer un fichier qui ne durera que pendant l'exécution du programme. Les programmeurs utilisent des fichiers temporaires pour stocker des données temporaires ou pour effectuer des opérations intermédiaires sans polluer leur système de fichiers.

## Comment faire:

```java
// Importer la classe File pour créer un fichier
import java.io.File;

// Utiliser la méthode createTempFile() pour créer un fichier temporaire
File tempFile = File.createTempFile("monfichier", ".txt");

// Utiliser la méthode getName() pour obtenir le nom du fichier temporaire
System.out.println("Nom du fichier temporaire: " + tempFile.getName());

// Utiliser la méthode write() pour écrire dans le fichier temporaire
PrintWriter writer = new PrintWriter(tempFile);
writer.write("Ceci est un exemple de texte dans le fichier temporaire.");
writer.close();
```

Output:
```
Nom du fichier temporaire: monfichier1234567890.txt
```

## Plongée en profondeur:

1. Contexte historique: La création de fichiers temporaires est devenue plus courante depuis la popularité des applications Web, où les fichiers temporaires sont utilisés pour stocker des sessions de navigation ou des téléchargements temporaires.
2. Alternatives: Les programmmeurs peuvent également utiliser la méthode deleteOnExit() pour demander au système de supprimer automatiquement le fichier temporaire à la fin du programme.
3. Détails de mise en oeuvre: Les fichiers temporaires sont généralement stockés dans le répertoire temporaire par défaut du système d'exploitation, mais il est également possible de spécifier un autre emplacement en utilisant la méthode createTempFile() avec les paramètres appropriés.

## Voir aussi:

- [Java Docs pour File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutoriel sur les fichiers temporaires en Java](https://www.baeldung.com/java-temporary-file)