---
title:                "Affichage des résultats de débogage"
html_title:           "Gleam: Affichage des résultats de débogage"
simple_title:         "Affichage des résultats de débogage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez probablement entendu parler de la célèbre phrase “print is not dead”. Eh bien, en programmation, c'est également vrai. L'impression de messages de débogage est un outil utile pour comprendre le comportement de votre code et trouver des erreurs.

## Comment Faire

L'utilisation de la fonction `debug` dans Gleam permet d'imprimer des messages de débogage dans votre code. Voici un exemple:

```Gleam
let name = "Jane"
debug("Hello", name)
```

Dans cet exemple, nous utilisons la fonction `debug` pour imprimer le message “Hello” suivi du contenu de la variable `name`. Lorsque vous exécutez votre code, le message “Hello Jane” sera imprimé dans votre terminal. Cela peut vous aider à suivre le flux de votre programme et à comprendre comment les valeurs changent au fil du temps.

Vous pouvez également utiliser des conditions pour n'imprimer des messages de débogage que lorsqu'une certaine condition est remplie. Par exemple:

```Gleam
let age = 25
if age > 18 {
    debug("Vous êtes un adulte!")
}
```

Dans cet exemple, le message “Vous êtes un adulte!” ne sera imprimé que si la variable `age` est supérieure à 18.

## Plongée Profonde

L'impression de messages de débogage peut également être utile pour vérifier si votre code fonctionne correctement. Vous pouvez utiliser des messages de débogage pour afficher le contenu de variables à des points spécifiques de votre code, afin de vous assurer que les valeurs sont correctes avant de continuer.

Il est important d'être sélectif dans les messages de débogage que vous imprimez, car trop de messages peuvent rendre votre code illisible. Utilisez-les avec parcimonie et n'oubliez pas de les supprimer une fois que vous avez corrigé vos erreurs.

## Voir Aussi

- [Documentation Gleam](https://gleam.run/documentation/guide/debugging)
- [Article Medium sur l'impression de messages de débogage en Gleam](https://medium.com/@middy/improve-your-debugging-skills-with-gleam-27e559f23185)
- [Guide de débogage en Gleam](https://matthewhammer.org/blog/2020/07/18/gleam-debugging.html)