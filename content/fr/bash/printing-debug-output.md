---
title:                "Impression de sortie de débogage"
html_title:           "Bash: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous faites du développement en Bash, vous êtes probablement déjà familier avec l'utilisation de la commande `echo` pour afficher du texte dans votre terminal. Mais saviez-vous que vous pouvez également utiliser cette commande pour afficher des messages de débogage? L'affichage de messages de débogage peut être utile pour comprendre le fonctionnement de votre script et identifier les erreurs éventuelles.

## Comment faire

Pour afficher des messages de débogage, vous devez utiliser la commande `echo` suivie du texte à afficher entre guillemets. Vous pouvez également inclure des variables dans votre message en utilisant la syntaxe `"$variable"`. Voici un exemple de code Bash qui affiche un message de débogage et la valeur d'une variable :

```Bash
#!/bin/bash
a=5
echo "La valeur de la variable a est : $a"
```

Lorsque vous exécutez ce script, le message suivant s'affiche dans votre terminal : 

```
La valeur de la variable a est : 5
```

Vous pouvez également utiliser la commande `printf` pour afficher des messages de débogage au lieu de `echo`. La syntaxe est légèrement différente, mais vous pouvez tout de même inclure des variables dans votre message. Voici un exemple :

```Bash
#!/bin/bash
b="Hello"
printf "Le contenu de la variable b est : %s\n" "$b"
```

Lors de l'exécution de ce script, vous obtiendrez le message suivant : 

```
Le contenu de la variable b est : Hello
```

## Plongée en profondeur

L'affichage de messages de débogage peut être utile pour comprendre le flux d'exécution de votre script, vérifier la valeur de vos variables à un certain point de celui-ci et repérer d'éventuelles erreurs. Vous pouvez également utiliser la commande `echo` ou `printf` dans des boucles ou des fonctions pour suivre l'évolution de vos données.

Il est important de noter que ces messages de débogage ne sont pas destinés à être utilisés dans votre code de production. Il est recommandé de les supprimer une fois que vous avez terminé de déboguer votre script.

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutoriel sur le débogage avec Bash](https://linuxhandbook.com/bash-debugging/)
- [Conseils pour le débogage en Bash](https://www.shellscript.sh/debugging.html)