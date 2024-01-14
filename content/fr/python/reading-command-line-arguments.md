---
title:    "Python: Lecture des arguments de la ligne de commande"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Il est important pour tout programmeur Python de connaître les bases de la lecture des arguments en ligne de commande. Cela vous permettra de créer des scripts plus flexibles et plus simples à utiliser. En comprenant comment lire les arguments en ligne de commande, vous pouvez également créer des programmes interactifs qui permettent à l'utilisateur de fournir des entrées personnalisées.

## Comment faire

Pour lire les arguments en ligne de commande en Python, vous pouvez utiliser le module "sys". Tout d'abord, importez le module en utilisant la ligne de code suivante : 
```Python
import sys
```

Ensuite, pour lire les arguments, utilisez la fonction "argv" en utilisant la syntaxe suivante : 
```Python
args = sys.argv
```

Maintenant, vous pouvez accéder à chaque argument individuellement en utilisant l'indexage. Par exemple, pour accéder au premier argument, utilisez : 
```Python
args[0]
```

En utilisant ces bases, vous pouvez maintenant écrire un programme qui vous permettra de fournir des arguments en ligne de commande et d'afficher leur valeur à l'écran.

## Plongée profonde

Il est important de noter que les arguments en ligne de commande sont toujours lus en tant que chaînes de caractères. Si vous souhaitez utiliser ces valeurs en tant que nombres ou autres types de données, vous devrez les convertir en utilisant des méthodes telles que "int()" ou "float()". De plus, vous pouvez également utiliser des modules tels que "argparse" pour gérer des arguments plus complexes et offrir une meilleure expérience utilisateur.

# Voir aussi

- Documentation officielle de Python sur les modules "sys" et "argparse" : https://docs.python.org/fr/3/library/sys.html, https://docs.python.org/fr/3/howto/argparse.html
- Un tutoriel sur la lecture des arguments en ligne de commande en Python : https://realpython.com/command-line-interfaces-python-argparse/
- Un exemple pratique de traitement d'arguments en ligne de commande en Python : https://www.geeksforgeeks.org/command-line-arguments-in-python/