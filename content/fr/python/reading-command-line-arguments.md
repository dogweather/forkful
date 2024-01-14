---
title:                "Python: Lecture des arguments en ligne de commande"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Python, vous savez probablement déjà à quel point les arguments de ligne de commande sont utiles. Mais pour ceux qui ne sont pas familiers avec ce concept, sachez que la lecture d'arguments de ligne de commande peut grandement faciliter l'exécution de votre code. Cela permet également à vos utilisateurs de personnaliser l'exécution de votre programme en fonction de leurs besoins.

## Comment faire

Lecture des arguments de ligne de commande en utilisant le module "sys". Tout d'abord, importez le module "sys" dans votre code :

```Python
import sys
```

Ensuite, vous pouvez extraire les arguments de ligne de commande en utilisant la fonction "argv" du module "sys" :

```Python
arguments = sys.argv
```

Cela vous donnera une liste d'arguments dans laquelle le premier élément est toujours le nom du fichier python lui-même. Vous pouvez ensuite parcourir cette liste pour obtenir les arguments spécifiques qui vous intéressent :

```Python
for i in range(1, len(arguments)):
    print("Argument #{} : {}".format(i, arguments[i]))
```

Voici un exemple de code complet :

```Python
import sys

arguments = sys.argv

for i in range(1, len(arguments)):
    print("Argument #{} : {}".format(i, arguments[i]))
```

Et voici un exemple de sortie en utilisant cette ligne de commande :

```
python mon_script.py argument1 argument2 argument3
```

```
Argument #1 : argument1
Argument #2 : argument2
Argument #3 : argument3
```

## Plongée en profondeur

Il est important de noter que les arguments de ligne de commande peuvent également être positionnels ou optionnels. Les arguments positionnels sont ceux qui doivent être fournis dans un ordre spécifique et doivent être utilisés par votre programme. Les arguments optionnels sont ceux qui sont facultatifs et peuvent être fournis dans n'importe quel ordre.

De plus, en utilisant le module "argparse", vous pouvez définir des arguments de ligne de commande avec des valeurs par défaut, des descriptions et même des options à choix multiples. Cela peut rendre votre code beaucoup plus clair et facile à utiliser pour vos utilisateurs.

Enfin, gardez à l'esprit que les arguments de ligne de commande peuvent également être utilisés pour fournir des informations de configuration à votre programme. Par exemple, vous pouvez utiliser un argument pour spécifier un fichier de configuration externe qui sera utilisé par votre code.

## Voir aussi

- [Documentation officielle Python pour le module "sys"](https://docs.python.org/fr/3/library/sys.html)
- [Documentation officielle Python pour le module "argparse"](https://docs.python.org/fr/3/library/argparse.html)
- [Article sur la lecture des arguments de ligne de commande en Python](https://realpython.com/command-line-interfaces-python-argparse/)