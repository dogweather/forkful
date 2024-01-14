---
title:                "Bash: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Bash, vous vous demandez peut-être pourquoi vous devriez vous intéresser à la lecture des arguments de la ligne de commande. En fait, la lecture des arguments de la ligne de commande peut être extrêmement utile pour automatiser des tâches récurrentes ou pour rendre votre script plus dynamique et flexible.

## Comment faire

Pour lire les arguments de la ligne de commande en Bash, vous devez utiliser la variable spéciale "$@", qui stocke tous les arguments passés au script lors de son exécution. Voici un exemple de code Bash pour lire ces arguments :

```Bash
#!/bin/bash

echo "Le premier agument est : $1"
echo "Le deuxième argument est : $2"
```

Lors de l'exécution de ce script avec les arguments "Bonjour" et "monde", le résultat sera :

```
$ ./script.sh Bonjour monde
Le premier agument est : Bonjour
Le deuxième argument est : monde
```

Vous pouvez également utiliser la boucle "for" pour parcourir tous les arguments passés. Voici un exemple :

```Bash
#!/bin/bash

for arg in "$@"
do
    echo "L'argument est : $arg"
done
```

Si vous exécutez ce script avec les arguments "Bonjour" "monde" "!" le résultat sera :

```
$ ./script.sh Bonjour monde !
L'argument est : Bonjour
L'argument est : monde
L'argument est : !
```

## Plongée en profondeur

Il est également possible de définir des options pour vos arguments de ligne de commande en utilisant la commande "getopt". Cela vous permet de rendre votre script plus convivial en permettant à l'utilisateur de spécifier des options pour personnaliser l'exécution du script. Voici un exemple de code Bash utilisant "getopt" :

```Bash
#!/bin/bash

# Définir les options disponibles
OPTIONS=abc

# Lire les options passées
OPTIND=1
while getopts $OPTIONS opt
do
    case $opt in
        a) echo "Option a sélectionnée" ;;
        b) echo "Option b sélectionnée" ;;
        c) echo "Option c sélectionnée" ;;
    esac
done
```

En exécutant ce script avec les options "a" "c", le résultat sera :

```
$ ./script.sh -ac
Option a sélectionnée
Option c sélectionnée
```

## Voir aussi
- [Documentation Bash - Variables spéciales de la ligne de commande](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
- [Guide du débutant Bash](https://www.tecmint.com/shell-scripting-hello-world-2/)
- [Documentation Bash - Utilisation de getopt](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)