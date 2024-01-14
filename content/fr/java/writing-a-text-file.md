---
title:    "Java: Ecrire un fichier texte"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Java ?

Il y a de nombreuses raisons pour lesquelles vous pourriez avoir besoin d'écrire un fichier texte dans votre programme Java. Par exemple, vous pourriez vouloir enregistrer les données saisies par l'utilisateur dans un formulaire, ou créer un fichier de configuration pour votre application.

## Comment le faire ?

Il est assez simple d'écrire un fichier texte en Java. Tout d'abord, vous devez créer un objet de type `File` en utilisant le chemin du fichier que vous souhaitez créer. Ensuite, vous pouvez utiliser un objet `FileWriter` pour écrire du texte dans ce fichier en utilisant la méthode `write()`. Enfin, n'oubliez pas de fermer le fichier avec la méthode `close()` pour vous assurer que les données sont correctement enregistrées.

```Java
File file = new File("monFichier.txt"); // créer le fichier
FileWriter writer = new FileWriter(file); // créer un objet FileWriter
writer.write("Bonjour tout le monde !"); // écrire dans le fichier
writer.close(); // fermer le fichier
```

Si vous voulez ajouter du texte à un fichier existant, vous pouvez utiliser un objet `FileWriter` avec un deuxième paramètre pour spécifier que vous voulez ajouter du contenu plutôt que de l'écraser :

```Java
File file = new File("monFichier.txt");
FileWriter writer = new FileWriter(file, true); // true pour ajouter du contenu
writer.write("\nC'est un plaisir d'écrire en Java !"); // ajouter du texte
writer.close();
```

## Plongée plus profonde

Il est important de noter que l'objet `FileWriter` peut lancer des exceptions, donc vous devrez les gérer ou les propager en utilisant une clause `throws` dans votre méthode. Vous pouvez également utiliser l'objet `BufferedWriter` pour écrire de gros volumes de données de manière plus efficace.

Il est également possible de formater le texte que vous écrivez en utilisant la classe `String.format()`. Enfin, n'oubliez pas d'ajouter des caractères de nouvelle ligne `\n` pour séparer les lignes de texte dans votre fichier.

## Voir aussi

Pour plus d'informations sur l'écriture de fichiers en Java, vous pouvez consulter ces liens :

- [Documentation officielle de Java pour la classe File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Guide sur la gestion des exceptions en Java](https://www.baeldung.com/java-exceptions)
- [Tutoriel sur l'utilisation de BufferedWriter en Java](https://www.codejava.net/java-se/file-io/java-bufferedwriter-tutorial-for-text-files)
- [Documentation officielle de Java pour la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)