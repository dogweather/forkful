---
title:                "La lecture d'un fichier texte"
html_title:           "Python: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
Lire un fichier texte signifie simplement lire le contenu d'un fichier texte dans un programme informatique. Les programmeurs font cela pour pouvoir manipuler ou traiter les données contenues dans ce fichier, telles que des mots, des nombres ou d'autres informations.

## Comment faire :
Voici comment lire un fichier texte en Python, en utilisant les outils intégrés dans le langage :

```Python
# Ouvrir un fichier en mode lecture
f = open("fichier.txt", "r")

# Lire le contenu du fichier
content = f.read()

# Afficher le contenu
print(content)

# Fermer le fichier
f.close()
```

Voici un exemple de contenu de fichier et sa sortie correspondante :

Contenu du fichier "fichier.txt":
```
Bonjour à tous !
Ça va bien ?
```

Sortie :
```
Bonjour à tous !
Ça va bien ?
```

## Plongée en profondeur :
Historiquement, les programmes devaient lire des fichiers texte car c'était le moyen le plus simple de stocker et de transmettre des données. Cependant, aujourd'hui, il existe d'autres formats de fichiers plus avancés qui peuvent être utilisés à la place, tels que les fichiers CSV ou JSON.

Pour lire un fichier texte en Python, nous utilisons la méthode `open()` qui retourne un objet `file`. Le mode `r` indique que le fichier sera ouvert en lecture, tandis que le mode `w` indique l'écriture. Après avoir obtenu l'objet `file`, nous pouvons utiliser la méthode `read()` pour lire le contenu du fichier. N'oublions pas de fermer le fichier après avoir terminé pour éviter les problèmes de mémoire.

## Voir aussi :
- [Documentation officielle de Python pour la méthode `open()`](https://docs.python.org/fr/3/library/functions.html#open)
- [Tutoriel sur la lecture de fichiers en Python](https://openclassrooms.com/fr/courses/235344-apprenez-a-programmer-en-python/234720-les-fichiers-2)