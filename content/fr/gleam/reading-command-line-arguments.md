---
title:                "Gleam: Lecture des arguments en ligne de commande"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi lire les arguments de ligne de commande en programmation Gleam ?

Si vous êtes un développeur en programmation Gleam, vous êtes sûrement familier avec l'utilisation de la ligne de commande pour exécuter vos programmes. Mais saviez-vous qu'il est également possible de lire des arguments de ligne de commande dans vos programmes ? Dans cet article, nous allons explorer pourquoi il peut être utile de le faire et comment le faire en utilisant le langage Gleam.

## Comment faire

Pour lire les arguments de ligne de commande en Gleam, nous allons utiliser la fonction `Gleam.Args.parse` qui prend un tableau de chaînes de caractères en entrée et retourne une structure de données contenant les arguments. Voici un exemple de code :

```Gleam 
import gleam/io

let args = Gleam.Args.parse(os.args)

gleam/io.info("Les arguments de la ligne de commande sont : (${args})")
```

Lorsque nous exécutons ce code avec la commande `gleam run monprogramme gleam`, nous obtenons le résultat suivant dans la console :

```
Les arguments de la ligne de commande sont : ([monprogramme, gleam])
```

Comme vous pouvez le voir, la fonction `parse` nous retourne un tableau avec le nom de notre programme en premier élément, suivi des arguments spécifiés lors de l'exécution. Vous pouvez ensuite utiliser ces arguments dans votre programme pour contrôler son comportement en fonction des paramètres donnés.

## Plongée en profondeur

La fonction `Gleam.Args.parse` a quelques options supplémentaires qui permettent de spécifier des arguments optionnels, de vérifier leur type et de leur donner des valeurs par défaut. Pour en savoir plus sur ces options, vous pouvez consulter la documentation officielle du langage Gleam.

De plus, il est important de noter que cette fonction ne fonctionne que pour les programmes Gleam compilés. Si vous souhaitez utiliser des arguments de ligne de commande dans votre code non compilé, vous devrez utiliser une bibliothèque tierce.

# Voir aussi

- [Documentation officielle du langage Gleam](https://gleam.run/book/tour/command-line-arguments.html)
- [Bibliothèque de traitement des arguments de ligne de commande en Gleam](https://github.com/gleam-lang/gleam-args)