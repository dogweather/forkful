---
title:                "Vérifier si un répertoire existe"
html_title:           "Java: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Découvrir si un répertoire existe est une opération simple mais cruciale en programmation Java, elle consiste à vérifié l'existence d'un emplacement de fichier spécifique. Les développeurs ont besoin de vérifier ces emplacements pour prévenir les erreurs dans leurs programmes, que ce soit pour empêcher de manipuler un fichier qui n'existe pas ou pour éviter l'écriture d'informations dans un répertoire inexistant.

## Comment faire:
Pour vérifier l'existence d'un répertoire en Java, on peut utiliser la classe `Files` en tandem avec `Paths`. Voici un exemple:

```Java
import java.nio.file.*;

public class DirectoryCheck {
    public static void main(String[] args) {
        Path dirPath = Paths.get("/mon/chemin/vers/le/dossier/");
        if (Files.exists(dirPath)) {
            System.out.println("Le répertoire existe.");
        } else {
            System.out.println("Le répertoire n'existe pas.");
        }
    }
}
```

Si le répertoire existe, vous verrez `"Le répertoire existe."`. Sinon, vous verrez `"Le répertoire n'existe pas."`.

## Plongée profonde
Avant Java 7, on utilisait la classe `File` pour accomplir la tâche de vérification de l'existence d'un répertoire. L'approche `Files` démontre une amélioration significative pour deux raisons principales: elle simplifie le code et fournit une meilleure gestion des erreurs.

En complément, pour vérifier si le chemin donné est non seulement existant mais est aussi un répertoire, vous pouvez utiliser la méthode `Files.isDirectory(Path)`. Cette méthode garantit que vous traitez réellement avec un répertoire et non avec un fichier.

La classe `Files` regorge d'autres méthodes utiles pour traiter avec des fichiers et des répertoires. Les développeurs Java sont encouragés à explorer la documentation officielle pour en savoir plus.

## Voir aussi
Pour une compréhension plus approfondie du sujet, vous pouvez visiter les liens suivants:
- Documentation Oracle : [Classe Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- Tutoriel Oracle sur les systèmes de fichiers : [Fichiers IO et NIO](https://docs.oracle.com/javase/tutorial/essential/io/notification.html)
- [StackOverflow](https://stackoverflow.com/questions/4871051/how-to-check-if-a-directory-exists-in-java) : Discussion sur la vérification de l'existence d'un dossier en Java.