---
title:                "Python: Lecture d'un fichier texte"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire des fichiers texte est une tâche courante dans la programmation Python. Cela peut être utile lorsque vous avez besoin de traiter de grandes quantités de données stockées dans un fichier texte, ou si vous devez extraire des informations spécifiques du texte. Dans cet article, nous allons examiner les différentes façons de lire un fichier texte avec Python et vous montrer comment tirer le meilleur parti de cette tâche.

## Comment faire

La première étape pour lire un fichier texte en Python est d'ouvrir le fichier en utilisant la fonction `open()`. Cette fonction prend deux arguments : le nom du fichier et le mode d'ouverture, qui peut être "r" pour lire le fichier en mode lecture. Voyons un exemple :

```Python
fichier = open("mon_fichier.txt", "r")
```

Une fois que vous avez ouvert le fichier, vous pouvez utiliser les méthodes `read()` ou `readline()` pour lire son contenu. La méthode `read()` renvoie tout le contenu du fichier sous forme de chaîne de caractères, tandis que la méthode `readline()` lit une seule ligne à la fois.

```Python
# Lire tout le contenu du fichier
contenu = fichier.read()
print(contenu)

# Lire une ligne à la fois
ligne = fichier.readline()
print(ligne)
```

Vous pouvez également lire le fichier ligne par ligne en utilisant une boucle `for` :

```Python
for ligne in fichier:
    # Faire quelque chose avec la ligne
    print(ligne)
```

N'oubliez pas de fermer le fichier après l'avoir lu en utilisant la méthode `close()` :

```Python
fichier.close()
```

## Plongée en profondeur

Outre les méthodes de base pour lire un fichier texte, il existe d'autres options plus avancées pour la manipulation de fichiers en Python. Par exemple, vous pouvez spécifier l'encodage du fichier en utilisant l'argument optionnel `encoding` de la fonction `open()`. Cela peut être utile si vous travaillez avec des fichiers dans des langues autres que l'anglais.

Vous pouvez également utiliser la structure `with` pour ouvrir le fichier, qui se chargera automatiquement de sa fermeture une fois que vous avez fini de l'utiliser :

```Python
with open("mon_fichier.txt", "r") as fichier:
    contenu = fichier.read()
    print(contenu)
```

Enfin, il existe des bibliothèques tierces telles que `csv` et `json` qui facilitent la lecture et la manipulation de fichiers de ces types spécifiques.

## Voir aussi

- [Documentation officielle sur la gestion des fichiers en Python](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel sur la lecture de fichiers en Python](https://www.programiz.com/python-programming/file-operation)
- [Bibliothèque csv pour la manipulation de fichiers CSV en Python](https://docs.python.org/fr/3/library/csv.html)