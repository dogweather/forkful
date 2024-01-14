---
title:    "Python: Lecture d'un fichier texte"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes intéressé par la programmation en Python, il est essentiel de savoir comment lire des fichiers texte. Les fichiers texte sont l'une des formes les plus courantes de données et la capacité de les lire efficacement peut grandement améliorer vos compétences en programmation.

# Comment faire

La première étape pour lire un fichier texte en Python est de l'ouvrir en utilisant la fonction ```open()```. Vous devrez fournir le nom du fichier ainsi que le mode d'ouverture (lecture, écriture, etc.). Par exemple :

```Python
fichier = open('exemple.txt', 'r')
```

Ensuite, vous pouvez utiliser la méthode ```read()``` pour lire le contenu du fichier. Cette méthode retournera une chaîne de caractères contenant le contenu du fichier. Par exemple :

```Python
contenu = fichier.read()
print(contenu)
```

Vous pouvez également utiliser la méthode ```readline()``` pour lire une seule ligne du fichier à la fois. Cette méthode est utile si vous travaillez avec de grands fichiers et que vous ne voulez pas charger tout le contenu en même temps. Par exemple :

```Python
ligne = fichier.readline()
print(ligne)
```

Lorsque vous avez terminé de lire le fichier, n'oubliez pas de le fermer en utilisant la méthode ```close()```. Cela libérera les ressources utilisées par le fichier. Par exemple :

```Python
fichier.close()
```

# Profonde plongée

Il existe plusieurs façons de lire des fichiers texte en Python, comme l'utilisation de boucles pour parcourir le fichier ligne par ligne ou l'utilisation de la méthode ```with``` pour gérer automatiquement la fermeture du fichier. Vous pouvez également utiliser des méthodes spécifiques pour lire un nombre spécifique de caractères ou pour lire le fichier dans une liste.

De plus, il est important de comprendre comment traiter les erreurs lors de la lecture d'un fichier texte. Parfois, le fichier peut ne pas exister ou ne pas avoir les autorisations nécessaires pour être lu, et dans ces cas, votre programme doit être capable de gérer ces erreurs de manière appropriée.

# Voir aussi

- [Documentation Python pour la lecture de fichiers](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Vidéo YouTube sur la lecture de fichiers en Python](https://www.youtube.com/watch?v=xlpriKFSjvc)
- [Guide pour lire des fichiers CSV en Python](https://realpython.com/python-csv/)