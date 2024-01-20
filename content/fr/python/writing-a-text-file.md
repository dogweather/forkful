---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire dans un fichier texte permet de sauvegarder des données. Les programmeurs le font pour enregistrer les résultats, configurer des programmes ou tracer des logs.

## Comment ça marche :
```Python
# Écriture simple dans un fichier
with open('exemple.txt', 'w') as fichier:
    fichier.write('Salut, amis codeurs !')

# Ajout de contenu à un fichier existant
with open('exemple.txt', 'a') as fichier:
    fichier.write('\nAjoutons une deuxième ligne.')
```
*Résultat - exemple.txt :*

```
Salut, amis codeurs !
Ajoutons une deuxième ligne.
```

## Exploration :
Historiquement, l'écriture de fichiers est l'une des opérations de base en programmation. Contrairement aux bases de données, les fichiers textes sont simples et universels, mais moins performants pour les gros volumes de données. Alternativement, on utilise les formats JSON ou XML pour structurer des données complexes. En Python, on manipule les fichiers avec des context managers (`with`) pour s'assurer qu'ils se ferment correctement.

## À voir aussi :
- [Documentation Python sur la gestion des fichiers](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel sur le module json pour la sérialisation](https://docs.python.org/3/library/json.html)
- [Approfondissement sur la gestion des fichiers en Python](https://realpython.com/read-write-files-python/)