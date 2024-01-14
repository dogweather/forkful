---
title:    "Fish Shell: Vérifier si un répertoire existe"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous vous demandez peut-être pourquoi il est important de vérifier si un répertoire existe en programmation Fish Shell. Eh bien, cela peut être utile pour s'assurer qu'un répertoire nécessaire existe avant d'y accéder ou d'y effectuer une action.

## Comment faire

Heureusement, vérifier si un répertoire existe en Fish Shell est assez simple. Utilisez simplement la commande `test` suivi de l'option `d` pour indiquer qu'il s'agit d'un répertoire, suivi du chemin du répertoire que vous souhaitez vérifier. Voici un exemple :

```Fish Shell
test d /chemin/vers/repertoire 
```

Si le répertoire existe, la sortie sera `true` et si ce n'est pas le cas, la sortie sera `false`.

## Plongeons plus en profondeur

Si vous souhaitez obtenir une sortie plus détaillée, vous pouvez utiliser la commande `ls` pour lister le contenu du répertoire et le filtrer en fonction de l'option `d` pour les répertoires. Par exemple :

```Fish Shell
ls -d /chemin/vers/repertoire
```

La sortie sera une liste des répertoires contenus dans le répertoire spécifié.

# Voir aussi

- [Documentation Fish Shell sur la commande `test`](https://fishshell.com/docs/current/cmds/test.html)
- [Documentation Fish Shell sur la commande `ls`](https://fishshell.com/docs/current/cmds/ls.html)