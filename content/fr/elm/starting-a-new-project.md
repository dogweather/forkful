---
title:    "Elm: Commencer un nouveau projet"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes intéressé par la programmation fonctionnelle et vous voulez commencer un nouveau projet en utilisant un langage moderne et élégant? Alors Elm peut être exactement ce que vous recherchez! Avec sa syntaxe claire et son système de typage fort, Elm est un choix populaire pour les développeurs cherchant à créer des applications web robustes et évolutives.

## Comment Faire

Pour commencer un nouveau projet Elm, la première étape est de télécharger et d'installer le compilateur Elm sur votre ordinateur. Vous pouvez le faire en suivant les instructions sur le site officiel d'Elm (https://guide.elm-lang.org/install/). Une fois que vous avez installé Elm, vous pouvez utiliser votre éditeur de code préféré pour créer un nouveau fichier avec l'extension ".elm".

Ensuite, vous pouvez commencer à écrire du code Elm! Voici un exemple de fonction qui prend un nombre en entrée et le multiplie par deux:

```Elm
double : Int -> Int
double num =
    num * 2
```

Vous pouvez également ajouter un bloc de code pour tester cette fonction et afficher le résultat dans la console:

```Elm
main =
    let
        number = 5
        result = double number
    in
        Html.text (toString result)
```

Après avoir enregistré votre fichier, ouvrez-le dans un navigateur et vous devriez voir le résultat "10" s'afficher.

## Profondeur de plongée

Maintenant que vous avez une compréhension de base de la façon de démarrer un projet Elm, vous pouvez aller plus profondément en explorant la documentation et les ressources en ligne. Le site officiel d'Elm (https://elm-lang.org/) a une section "Guide" qui vous fournira des informations détaillées sur le langage et ses fonctionnalités. Vous pouvez également rejoindre la communauté active d'Elm sur le forum (https://discourse.elm-lang.org/) pour poser des questions et obtenir de l'aide.

## Voir Aussi

- Site officiel d'Elm: https://elm-lang.org/
- Installation du compilateur Elm: https://guide.elm-lang.org/install/
- Forum Elm: https://discourse.elm-lang.org/