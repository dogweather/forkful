---
title:                "Python: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être déjà entendu parler de l'expression "arguments de ligne de commande" en programmation Python, mais vous n'êtes pas sûr de savoir exactement ce qu'elle signifie ou pourquoi elle est importante. Dans cet article, nous allons expliquer pourquoi la lecture des arguments de ligne de commande est essentielle pour les programmeurs Python.

## Comment faire

Pour lire les arguments de ligne de commande dans Python, nous utilisons le module `sys`. Voici un exemple de code qui illustre comment utiliser ce module :

```Python
import sys

# Récupérer les arguments de ligne de commande
arguments = sys.argv

# Imprimer chaque argument sur une nouvelle ligne
for arg in arguments:
    print(arg)
```

Si nous exécutons ce code avec des arguments de ligne de commande comme ceci : `python mon_code.py arg1 arg2 arg3`, nous obtiendrons l'affichage suivant :

```
mon_code.py
arg1
arg2
arg3
```

Comme vous pouvez le voir, la liste `sys.argv` contient le nom du fichier Python que nous avons exécuté en premier, suivi de tous les arguments que nous avons passés à la suite.

## Plongée profonde

Maintenant que nous savons comment lire les arguments de ligne de commande en Python, explorons quelques cas d'utilisation courants pour cette fonctionnalité.

### Utilisation de arguments pour exécuter différentes actions

L'un des avantages de la lecture des arguments de ligne de commande est que cela nous permet d'ajouter de la flexibilité à nos programmes. Nous pouvons utiliser les arguments pour déterminer quelle action doit être exécutée. Par exemple, nous pouvons modifier notre code pour imprimer un message différent en fonction du premier argument passé :

```Python
import sys

if sys.argv[1] == "bonjour":
    print("Salut tout le monde !")
elif sys.argv[1] == "au revoir":
    print("À bientôt !")
```

Maintenant, si nous exécutons notre code avec `python mon_code.py bonjour`, nous obtiendrons l'affichage `Salut tout le monde !`, tandis que `python mon_code.py au revoir` affichera `À bientôt !`.

### Utilisation d'arguments pour fournir des options

Les arguments de ligne de commande peuvent également être utilisés pour fournir des options à un programme. Par exemple, nous pourrions utiliser des arguments pour déterminer si nous voulons imprimer un message en majuscules ou en minuscules :

```Python
import sys

if sys.argv[1] == "-majuscules":
    print("MON MESSAGE EN MAJUSCULES !")
elif sys.argv[1] == "-minuscules":
    print("mon message en minuscules !")
```

En exécutant notre code avec `python mon_code.py -majuscules`, nous obtenons `MON MESSAGE EN MAJUSCULES !`, tandis que `python mon_code.py -minuscules` affichera `mon message en minuscules !`.

### Utilisation d'arguments pour lire des fichiers

Enfin, les arguments de ligne de commande peuvent être utiles pour lire des fichiers spécifiés par l'utilisateur. Par exemple, nous pourrions modifier notre code pour lire et imprimer le contenu d'un fichier spécifié par un argument :

```Python
import sys

# Récupérer le nom du fichier à lire depuis le deuxième argument
nom_fichier = sys.argv[2]

# Ouvrir et lire le fichier
with open(nom_fichier, 'r') as f:
    contenu = f.read()
    print(contenu)
```

Si nous exécutons notre code avec `python mon_code.py lire fichier.txt`, notre programme lira et imprimera le contenu du fichier "fichier.txt".

## Voir aussi

Pour en savoir plus sur la manipulation des arguments en Python, consultez ces ressources :

- [Documentation officielle de Python sur le module `sys`](https://docs.python.org/fr/3/library/sys.html)
- [Tutoriel RealPython : Arguments de ligne de commande en Python](https://realpython.com/python-command-line-arguments/)

Nous espérons que cet article vous a donné une meilleure compréhension des arguments de ligne de commande en Python et de leurs utilisations pratiques. N'hésitez pas à explorer davantage et à expérimenter avec cette fonctionnalité pour devenir un programmeur Python plus efficace !