---
title:    "Python: La lecture d'un fichier texte"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte est essentiel pour tout programmeur Python. Cela permet de manipuler et de traiter des données provenant de différentes sources, telles que des fichiers de données ou des pages web. Dans cet article, nous allons vous montrer comment lire un fichier texte en utilisant Python.

## Comment faire

Pour commencer, il est important d'ouvrir le fichier texte en mode lecture en utilisant la fonction `open()`. Cette fonction prend deux paramètres, le nom du fichier et le mode (dans notre cas, "r" pour lecture). Par exemple :

```Python
fichier = open("mon_fichier.txt", "r")
```

Ensuite, nous pouvons utiliser la méthode `read()` pour lire le contenu du fichier et le stocker dans une variable. Cette méthode peut être utilisée avec ou sans paramètre. Si aucun paramètre n'est passé, elle lira tout le contenu du fichier, sinon elle lira seulement le nombre de caractères spécifié. Voici un exemple :

```Python
fichier = open("mon_fichier.txt", "r")
contenu = fichier.read()
```

Nous pouvons également utiliser une boucle `for` pour lire le contenu du fichier ligne par ligne. Dans cet exemple, nous allons utiliser la méthode `readline()` qui lit une ligne à la fois :

```Python
fichier = open("mon_fichier.txt", "r")
for ligne in fichier:
    print(ligne)
```

Une fois que nous avons fini de lire le fichier, il est important de le fermer en utilisant la méthode `close()` pour libérer les ressources. Voici un exemple complet avec une gestion d'erreurs :

```Python
try:
    fichier = open("mon_fichier.txt", "r")
    contenu = fichier.read()
    print(contenu)
except FileNotFoundError:
    print("Le fichier spécifié n'a pas été trouvé.")
finally:
    fichier.close()
```

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, Python offre de nombreuses autres options pour lire un fichier texte. Par exemple, la méthode `readlines()` permet de stocker chaque ligne du fichier dans une liste, facilitant ainsi le traitement des données. De plus, il est possible de spécifier un encodage particulier lors de l'ouverture du fichier en utilisant le paramètre `encoding`. Cela peut être utile lorsque vous avez affaire à des caractères spéciaux.

## Voir aussi

- [Documentation officielle Python pour la gestion des fichiers](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel vidéo : Comment lire un fichier texte avec Python](https://www.youtube.com/watch?v=N2kLEZpMvmI)
- [Article sur l'encodage en Python](https://realpython.com/python-encodings-guide/)