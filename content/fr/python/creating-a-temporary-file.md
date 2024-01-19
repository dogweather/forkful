---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La création d'un fichier temporaire consiste à créer un fichier qui sera utilisé de manière transitoire pendant l'exécution d'un programme. Les programmeurs le font principalement pour stocker de manière sécurisée des données temporaires qui ne sont pas nécessaires une fois le programme terminé.

## Comment faire:
Le module tempfile de Python facilite cette tâche. Voyons un exemple:

```Python
import tempfile

with tempfile.TemporaryFile() as tempf:
    tempf.write(b'Quelques données ici')  # Écrire dans le fichier temporaire
    tempf.seek(0)  # Repositionner le curseur au début du fichier
    print(tempf.read())  # Lire le contenu du fichier

# En sortie, vous verrez
# Quelques données ici
```
Comme vous pouvez le voir, le fichier temporaire est automatiquement supprimé une fois fermé.

## Plongée en profondeur
Historiquement, gérer des fichiers temporaires était plus difficile car il fallait gérer manuellement leur suppression. Python a introduit le module tempfile pour rendre cette tâche plus facile et plus sûre.

Il existe des alternatives à tempfile, comme la création manuelle d'un fichier avec os et shutil, mais c'est plus complexe et nécessite de gérer la suppression du fichier vous-même. Tempfile est donc une solution performante et conviviale.

Tempfile crée les fichiers dans un répertoire spécifique à la plate-forme, généralement /tmp sur Linux et MacOS, et C:\Users\\<user>\AppData\Local\Temp sur Windows. Vous pouvez modifier cela en définissant la variable d'environnement TMPDIR.

## Voir aussi
Voici quelques liens vers des sources connexes pour en savoir plus:
- [Documentation Python pour tempfile](https://docs.python.org/fr/3/library/tempfile.html)
- [Tutorial sur Real Python](https://realpython.com/python-tempfile/)
- [Post StackOverflow](https://stackoverflow.com/questions/3924117/how-to-use-tempfile-in-python)