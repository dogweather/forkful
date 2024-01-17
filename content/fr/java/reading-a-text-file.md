---
title:                "Lecture d'un fichier texte"
html_title:           "Java: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi? 
Lire un fichier texte en programmation consiste simplement à accéder et à parcourir le contenu d'un fichier texte. Les programmeurs le font souvent pour extraire des données ou pour les manipuler dans leurs programmes.

## Comment faire:
Voici un exemple de code en Java pour lire un fichier texte et imprimer son contenu:

```Java
// Importer la classe File et la classe Scanner 
import java.io.File;
import java.util.Scanner;

// Déclarer le fichier à lire
File fichier = new File("monfichier.txt");

try {
  // Utiliser Scanner pour parcourir le contenu du fichier ligne par ligne
  Scanner scanner = new Scanner(fichier);
  while (scanner.hasNextLine()) {
    String ligne = scanner.nextLine();
    // Imprimer chaque ligne du fichier
    System.out.println(ligne);
  }
  // Fermer le scanner
  scanner.close();
} catch (Exception e) {
  e.printStackTrace();
}
```

Si le contenu de notre fichier texte est le suivant:
```
Bonjour
Comment allez-vous?
Je vais bien merci.
Et vous?
```

Alors la sortie du programme sera:
```
Bonjour
Comment allez-vous?
Je vais bien merci.
Et vous?
```

## Analyse approfondie:
La lecture de fichiers texte est un concept fondamental en programmation qui existe depuis les premiers langages de programmation. Alternativement, les programmeurs peuvent également utiliser des bibliothèques ou des outils spécialisés pour lire et manipuler des fichiers texte.

## Voir aussi:
Pour plus d'informations sur la lecture de fichiers texte en Java, vous pouvez consulter la documentation officielle de Java sur la classe File [ici](https://docs.oracle.com/javase/8/docs/api/java/io/File.html) et la classe Scanner [ici](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html). Vous pouvez également trouver des ressources utiles sur la manipulation de fichiers texte sur [Stack Overflow](https://stackoverflow.com/questions/4716503/reading-a-plain-text-file-in-java).