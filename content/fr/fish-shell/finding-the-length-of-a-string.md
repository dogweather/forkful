---
title:    "Fish Shell: Trouver la longueur d'une chaîne"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Que ce soit pour compter le nombre de lettres dans un mot ou pour vérifier si une entrée utilisateur respecte un certain format, savoir comment obtenir la longueur d'une chaîne est une compétence essentielle pour tout développeur.

## Comment faire

Heureusement, avec Fish Shell, obtenir la longueur d'une chaîne est très simple. Il suffit d'utiliser la commande `string` avec l'option `-l`, suivi de la chaîne à évaluer.

```
Fish Shell: string -l "Bonjour"
```

Cette commande renverra la longueur de la chaîne "Bonjour", qui est de 7 caractères. Si vous souhaitez stocker cette valeur dans une variable, vous pouvez le faire de la manière suivante :

```
Fish Shell: set longueur (string -l "Bonjour")
```

Maintenant, vous pouvez utiliser cette variable `longueur` dans votre code, par exemple pour vérifier si une entrée utilisateur est assez longue.

## Plongée en profondeur

Maintenant que vous savez comment trouver la longueur d'une chaîne en utilisant Fish Shell, il est intéressant de comprendre comment cela fonctionne réellement. En fait, la commande `string -l` utilise la fonction interne `__fish_len_string` pour calculer la longueur de la chaîne. Cette fonction utilise un algorithme efficace pour parcourir tous les caractères de la chaîne et compter leur nombre. 

Il est important de noter que cette fonction ne compte que les caractères utilisant un seul code UTF-8. Les caractères combinés, tels que les accents ou les emojis, seront comptés comme un seul caractère. Si vous voulez une fonction plus précise pour compter ces combinaisons de caractères, vous pouvez utiliser la commande `string -c` à la place.

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/string.html)
- [La page Wikipédia sur UTF-8](https://fr.wikipedia.org/wiki/UTF-8)
- [Des exemples de commandes utiles en Fish Shell](https://fishery.fish/tutorial/cheatsheet/)