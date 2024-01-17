---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Java: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

Vérifier si un répertoire existe est une tâche courante pour les programmeurs Java. Cela leur permet de s'assurer qu'un chemin vers un répertoire est valide avant de l'utiliser dans leur code. Cela peut également être utile pour manipuler des fichiers et des dossiers dans un répertoire spécifique.

## Comment:

Voici un exemple de code Java pour vérifier si un répertoire existe:

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // définit le chemin du répertoire à vérifier
        String chemin = "chemin/vers/mon/répertoire";
        
        // crée un objet File en utilisant le chemin
        File rep = new File(chemin);
        
        // vérifie si le répertoire existe
        if (rep.exists()) {
            System.out.println("Le répertoire existe!");
        } else {
            System.out.println("Le répertoire n'existe pas!");
        }
    }
}
```

Si le répertoire existe, le programme affichera "Le répertoire existe!" Sinon, il affichera "Le répertoire n'existe pas!"

## Approfondissement:

Vérifier si un répertoire existe est une tâche courante depuis les premières versions de Java. Cependant, avec l'introduction de Java 7, il existe une façon plus élégante d'accomplir cette tâche en utilisant la classe "Path". Par exemple:

```Java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class CheckDirectory {
    public static void main(String[] args) {
        // définit le chemin du répertoire à vérifier
        Path chemin = Paths.get("chemin/vers/mon/répertoire");
        
        // vérifie si le répertoire existe
        if (Files.exists(chemin)) {
            System.out.println("Le répertoire existe!");
        } else {
            System.out.println("Le répertoire n'existe pas!");
        }
    }
}
```

Cette méthode est plus efficace et offre plus d'options pour la manipulation des fichiers et des répertoires.

## Voir aussi:

Pour plus d'informations sur la classe Path, vous pouvez consulter la documentation officielle de Java: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html

Pour une comparaison entre les deux méthodes pour vérifier si un répertoire existe, vous pouvez consulter cet article: https://www.baeldung.com/java-check-if-file-exists