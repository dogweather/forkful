---
title:                "Kotlin: Lecture des arguments de ligne de commande"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture des arguments de ligne de commande est une compétence essentielle pour tout programmeur Kotlin. Cela vous permet de passer des paramètres à votre programme lors de son exécution, ce qui le rend plus dynamique et lui permet de traiter différents scénarios en fonction des entrées fournies. Que vous soyez développeur de logiciels ou étudiant en informatique, comprendre comment lire les arguments de ligne de commande vous sera utile dans de nombreuses situations.

## Comment faire

Pour lire les arguments de ligne de commande en Kotlin, vous devez suivre ces étapes simples :

1. Tout d'abord, créez une fonction `main()` pour votre programme Kotlin. C'est la fonction principale qui sera exécutée lorsque vous lancerez votre programme.

```Kotlin
fun main(args: Array<String>) {
    // Votre code ici
}
```

2. Ensuite, utilisez le paramètre `args` de type `Array<String>` pour accéder à la liste des arguments de ligne de commande. Vous pouvez itérer à travers cette liste pour traiter chaque argument individuellement ou les utiliser directement.

```Kotlin
fun main(args: Array<String>) {
    // Affiche tous les arguments de ligne de commande
    args.forEach { println(it) }
}
```

3. Pour exécuter votre programme avec des arguments de ligne de commande, ouvrez une fenêtre de terminal, accédez à l'emplacement de votre fichier Kotlin et utilisez la commande `kotlin <nom_fichier>.kt <arguments>`. Les arguments doivent être séparés par des espaces.

```
kotlin monprogramme.kt arg1 arg2 arg3
```

4. Vous pouvez également passer des arguments optionnels en utilisant des drapeaux. Par exemple, `-f <nom_fichier>` ou `--input <valeur>`.

```Kotlin
fun main(args: Array<String>) {
    // Vérifie si le drapeau -v ou --verbose a été passé comme argument
    if ("-v" in args || "--verbose" in args) {
        println("Mode verbose activé")
    }
}
```

## Plongée en profondeur

Lors de la lecture des arguments de ligne de commande en Kotlin, il est important de prendre en compte certaines choses :

- Les arguments de ligne de commande sont transmis à votre programme en tant que chaînes de caractères, donc vous devez les convertir en types de données appropriés si nécessaire.
- Si vous n'avez pas besoin de traiter les arguments un par un et que vous souhaitez simplement en récupérer une liste, vous pouvez utiliser `args.toList()`.
- N'oubliez pas de gérer les erreurs et les cas où aucun argument n'est fourni lors de l'exécution de votre programme.

## Voir aussi

- [Documentation Kotlin sur les arguments de ligne de commande](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Guide sur les arguments de ligne de commande en Kotlin](https://www.baeldung.com/kotlin/command-line-arguments)