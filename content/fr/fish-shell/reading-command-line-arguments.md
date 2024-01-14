---
title:                "Fish Shell: Lecture des arguments de ligne de commande"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur, vous savez probablement déjà l'importance de la ligne de commande dans votre travail quotidien. Mais saviez-vous que vous pouvez utiliser des arguments de ligne de commande pour rendre vos scripts encore plus puissants et flexibles? Dans cet article, nous allons examiner comment lire les arguments de ligne de commande dans Fish Shell et comment en tirer le meilleur parti dans vos projets.

## Comment faire

Pour lire les arguments de ligne de commande dans Fish Shell, vous pouvez utiliser la variable spéciale `$argv`. Cette variable contient une liste des arguments passés à votre script, avec le premier argument étant `$argv[1]`, le deuxième étant `$argv[2]`, et ainsi de suite.

````Fish Shell
#!/usr/bin/fish

# Lire et afficher le premier argument
echo "Le premier argument est: $argv[1]"

# Afficher une erreur si aucun argument n'est passé
if test -z "$argv"
    echo "Veuillez fournir au moins un argument!"
    exit 1
end
````

Supposons que vous enregistrez ce script sous le nom `test.fish` et l'exécutez en utilisant `fish test.fish argument1 argument2`. Vous verrez alors la sortie suivante:

````Shell
Le premier argument est: argument1
````

Vous pouvez également utiliser la fonction `count` pour vérifier le nombre d'arguments passés à votre script:

````Fish Shell
#!/usr/bin/fish

# Nombre d'arguments
echo "Nombre d'arguments: (count $argv)"
````

La sortie de ce script sera:

````Shell
Nombre d'arguments: 2
````

Maintenant que vous pouvez lire les arguments de ligne de commande dans Fish Shell, vous pouvez les utiliser pour rendre vos scripts plus dynamiques et réutilisables. Vous pouvez également les combiner avec d'autres fonctions Shell pour une meilleure expérience de développement.

## Deep Dive

En plus de la variable `$argv`, il existe également des options intégrées dans Fish Shell pour travailler avec des arguments de ligne de commande. Par exemple, vous pouvez utiliser la fonction `argv` pour récupérer une liste complète des arguments passés à votre script, ou `argv0` pour récupérer le nom du script en cours d'exécution.

De plus, vous pouvez utiliser `set -l` pour définir une variable locale à l'intérieur d'un script, ce qui peut être très utile pour traiter des arguments spécifiques.

Pour en savoir plus sur la lecture des arguments de ligne de commande dans Fish Shell, vous pouvez toujours vous référer à la documentation officielle de Fish Shell ou explorer d'autres scripts et projets existants pour voir comment ils utilisent des arguments de ligne de commande.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Exemples de scripts Fish Shell](https://github.com/fish-shell/fish-shell/tree/main/examples)