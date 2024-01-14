---
title:                "Bash: Lecture des arguments de ligne de commande"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash ou que vous cherchez simplement à améliorer vos compétences en programmation, la lecture des arguments de ligne de commande peut être une compétence utile à apprendre. Cela peut vous permettre de créer des scripts plus dynamiques et personnalisables, ainsi que de mieux comprendre comment votre programme interagit avec les entrées de l'utilisateur.

## Comment faire

Pour lire les arguments de ligne de commande en Bash, vous devez utiliser la variable spéciale `$@` qui contient tous les arguments passés au script. Vous pouvez également utiliser les variables numérotées `$1, $2, $3`, etc. pour accéder à un argument spécifique.

Voici un exemple simple de code qui affiche tous les arguments passés au script :

```Bash
#!/bin/bash
echo "Les arguments passés sont : $@"
```

Lorsque vous exécutez ce script avec des arguments séparés par des espaces, comme `./script.sh argument1 argument2`, la sortie sera :

```
Les arguments passés sont : argument1 argument2
```

Vous pouvez également utiliser la commande `shift` pour parcourir tous les arguments passés et les utiliser dans des boucles ou des conditions.

## Plongée en profondeur

L'une des fonctionnalités les plus utiles de la lecture des arguments de ligne de commande est la possibilité de les valider et de les utiliser dans des conditions. Par exemple, vous pouvez vérifier si un certain argument a été passé et, si c'est le cas, effectuer une tâche spécifique.

Voici un exemple de code qui utilise un argument pour vérifier si un fichier existe et l'affiche si c'est le cas :

```Bash
#!/bin/bash
if [ -f $1 ]
then
  echo "Le fichier $1 existe."
else
  echo "Le fichier $1 n'existe pas."
fi
```

Si vous exécutez ce script avec un argument correspondant à un fichier existant, comme `./script.sh fichier.txt`, la sortie sera :

```
Le fichier fichier.txt existe.
```

Sinon, si l'argument ne correspond pas à un fichier existant, la sortie sera :

```
Le fichier fichier.txt n'existe pas.
```

En utilisant des arguments de ligne de commande, vous pouvez également rendre votre script plus interactif en demandant à l'utilisateur de saisir certaines valeurs au lieu de les définir directement dans le code.

## Voir aussi

- [La documentation Bash officielle](https://www.gnu.org/software/bash/manual/bash.html)
- [Un tutoriel interactif pour apprendre Bash](https://www.learnshell.org/)
- [Un guide détaillé sur la lecture des arguments de ligne de commande en Bash](https://linuxize.com/post/bash-script-get-arguments/)