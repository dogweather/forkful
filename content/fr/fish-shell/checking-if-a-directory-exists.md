---
title:                "Fish Shell: Vérification de l'existence d'un répertoire"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi vérifier si un répertoire existe avec Fish Shell

Lors de la programmation en Fish Shell, il peut être utile de vérifier si un répertoire existe avant d'effectuer une action. Cela peut vous éviter des erreurs et des temps d'exécution inutiles en vous assurant que le répertoire nécessaire est bien présent avant de l'utiliser.

## Comment faire

Il existe une commande simple en Fish Shell pour vérifier si un répertoire existe :

```Fish Shell
test -d /chemin/vers/repertoire
```

Cette commande renverra "vrai" si le répertoire existe et "faux" dans le cas contraire. Vous pouvez également utiliser l'opérateur logique "&&" pour effectuer une action seulement si le répertoire existe :

```Fish Shell
test -d /chemin/vers/repertoire && echo "Le répertoire existe !"
```

## Plongée en profondeur

Il est également possible d'utiliser plusieurs options avec la commande "test" pour vérifier l'existence d'un répertoire. Par exemple, vous pouvez utiliser l'option "-e" pour vérifier si un répertoire ou un fichier existe :

```Fish Shell
test -e /chemin/vers/dossier
```

Vous pouvez également utiliser l'opérateur de négation "!" pour vérifier si un répertoire n'existe pas :

```Fish Shell
! test -d /chemin/vers/repertoire && echo "Le répertoire n'existe pas."
```

## Voir aussi

- [Documentation Fish Shell sur la commande "test"](https://fishshell.com/docs/current/cmds/test.html)
- [Tutoriel Fish Shell en français](https://www.supinfo.com/articles/single/168-tutoriel-fish-shell)
- [Fish Shell et la programmation shell dans un contexte de développement web](https://elephant-carpaccio.ch/?p=404)

# Voir également