---
title:                "Java: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous savez probablement que les ordinateurs stockent les fichiers et dossiers dans des emplacements spécifiques appelés "répertoires". Mais saviez-vous qu'il est important de vérifier si un répertoire existe avant d'interagir avec? Dans cet article, nous allons explorer pourquoi la vérification de l'existence d'un répertoire est essentielle dans la programmation Java.

## Comment faire

Pour vérifier si un répertoire existe en Java, nous utiliserons la méthode `exists()` de la classe `File`. Voici un exemple de code qui montre comment l'utiliser:

```Java
import java.io.File;

public class Main {
  public static void main(String[] args) {
    // Créer un objet File avec le chemin du répertoire à vérifier
    File directory = new File("chemin/vers/mon/repertoire");

    if(directory.exists()){ // Vérifie si le répertoire existe
      System.out.println("Le répertoire existe!");
    } else {
      System.out.println("Le répertoire n'existe pas.");
    }
  }
}
```

En utilisant la méthode `exists()` et une simple instruction `if-else`, nous pouvons facilement vérifier l'existence d'un répertoire et agir en conséquence dans notre code.

Voici un exemple de sortie pour un répertoire existant:

```
Le répertoire existe!
```

Et voici un exemple de sortie pour un répertoire inexistant:

```
Le répertoire n'existe pas.
```

## Plongée en profondeur

Il est important de vérifier si un répertoire existe pour plusieurs raisons. Tout d'abord, cela garantit que vous interagissez uniquement avec des fichiers ou des dossiers qui existent réellement, évitant ainsi les erreurs et les exceptions inutiles. Deuxièmement, cela peut être une étape importante dans la création ou la gestion de fichiers et de dossiers dans votre application. Par exemple, si vous devez créer un nouveau fichier ou copier un fichier vers un répertoire spécifique, la première étape serait de vérifier si ce répertoire existe déjà.

De plus, la méthode `exists()` peut également être utilisée pour vérifier l'existence de fichiers individuels. Cela peut être utile dans des scénarios où vous devez vérifier si un fichier existe avant de le lire ou de l'écrire.

Il est également bon de noter que la méthode `exists()` renvoie `true` si l'élément en question existe, qu'il s'agisse d'un fichier, d'un répertoire ou d'un lien symbolique.

## Voir aussi

- [Java File Class Documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java File class tutorial](https://www.baeldung.com/java-file)
- [Java IO Tutorial](https://www.tutorialspoint.com/java/java_files_io.htm)