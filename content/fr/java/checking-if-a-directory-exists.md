---
title:    "Java: Vérification de l'existence d'un répertoire"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une tâche importante pour de nombreux programmeurs Java. Cela peut être utile dans des situations où vous devez vous assurer qu'un répertoire spécifié existe avant de procéder à des opérations telles que la création de fichiers ou l'exécution d'autres tâches.

## Comment faire

La vérification de l'existence d'un répertoire en Java peut être facilement réalisée en utilisant la classe File de Java et sa méthode `exists()`. Voici un exemple de code qui vérifie si un répertoire nommé "documents" existe :

```Java
File directory = new File("documents");
if(directory.exists()) {
    System.out.println("Le répertoire documents existe !");
} else {
    System.out.println("Le répertoire documents n'existe pas !");
}
```

Lorsque ce code est exécuté, si le répertoire "documents" existe, le message "Le répertoire documents existe !" sera imprimé à la console. Sinon, le message "Le répertoire documents n'existe pas !" sera affiché.

## Plongée en profondeur

La méthode `exists()` vérifie si le fichier ou le répertoire spécifié existe sur le système de fichiers actuel. Elle renvoie `true` si le fichier ou le répertoire existe et `false` sinon. La méthode `exists()` ne fait pas la distinction entre les fichiers et les répertoires, elle peut donc être utilisée pour vérifier l'existence de n'importe quel type de fichier.

Il est également important de noter que la méthode `exists()` ne garantit pas que les permissions nécessaires pour accéder au fichier ou au répertoire spécifié sont disponibles. Elle ne vérifie que l'existence du fichier ou du répertoire.

## Voir aussi

- La classe File de Java : https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- L'article "Vérifier si un répertoire existe en Java" sur CodeGym : https://codegym.cc/quests/lectures/fr/questions/tutorial/java/check-directory-exists