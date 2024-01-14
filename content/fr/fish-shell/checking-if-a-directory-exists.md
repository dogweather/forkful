---
title:    "Fish Shell: Vérifier si un répertoire existe"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi
Il est important de vérifier si un répertoire existe lors de la programmation en shell pour éviter les erreurs inattendues et assurer que le script s'exécute correctement.

## Comment faire
Pour vérifier si un répertoire existe en utilisant Fish Shell, vous pouvez utiliser la commande `test -d` suivie du chemin du répertoire. Voici un exemple :

```Fish Shell
test -d /chemin/vers/le/répertoire
```

Si le répertoire existe, la sortie sera `true`, sinon elle sera `false`.

## Plongée en profondeur
Lorsque la commande `test -d` est exécutée, elle recherche le répertoire spécifié et renvoie `true` si elle le trouve et `false` sinon. Cependant, si le répertoire n'est pas trouvé, une erreur peut être générée. Pour éviter cela, vous pouvez utiliser `if` et `else` pour gérer la sortie de la commande. Voici un exemple :

```Fish Shell
if test -d /chemin/vers/le/répertoire
    echo "Le répertoire existe !"
else
    echo "Le répertoire n'existe pas."
end
```

Vous pouvez également utiliser `not` pour inverser la sortie. Voici un exemple :

```Fish Shell
if not test -d /chemin/vers/le/répertoire
    echo "Le répertoire n'existe pas."
else
    echo "Le répertoire existe !"
end
```

## Voir aussi
- [Documentation de test en Fish Shell] (https://fishshell.com/docs/current/cmds/test.html)
- [Bash: Vérifier si un répertoire existe] (https://linuxize.com/post/bash-check-if-directory-exists/)