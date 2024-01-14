---
title:    "Python: Écrire un fichier texte"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Python?

Écrire un fichier texte en Python est utile lorsque vous avez besoin de stocker des données de manière organisée et facilement accessible. Cela peut être utile dans de nombreux cas, tels que la création d'un fichier de configuration pour un programme ou la sauvegarde de données pour une analyse ultérieure.

## Comment faire pour écrire un fichier texte en Python?

Voici un exemple simple de code Python pour créer un fichier texte et y écrire des données:

```python
# Ouvrir le fichier en mode écriture
fichier = open("nouveau-fichier.txt", "w")

# Écrire des données dans le fichier
fichier.write("Ceci est une première ligne dans mon fichier.")
fichier.write("Et voici une deuxième ligne.")

# Fermer le fichier
fichier.close()

# Vérifier le contenu du fichier
print("Contenu du fichier:")
fichier = open("nouveau-fichier.txt", "r")
print(fichier.read())

# Fermer le fichier
fichier.close()
```

Cela créera un fichier texte nommé "nouveau-fichier.txt" dans le même dossier que votre code Python. Vous pouvez changer le nom et le chemin du fichier selon vos besoins.

## Plongée en profondeur

En écrivant un fichier texte en Python, il est important de noter que le mode d'ouverture du fichier peut avoir un impact sur le contenu existant. En utilisant le mode "w", le contenu précédent sera supprimé et écrasé par les nouvelles données. Cela peut être évité en utilisant le mode "a" pour ajouter du contenu à la fin du fichier.

Il est également possible d'écrire des données formatées en utilisant la fonction "format()" en Python pour rendre le fichier plus lisible et organisé.

## Voir aussi

- [Documentation officielle Python pour l'écriture dans un fichier](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel pour écrire un fichier texte en Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [Différentes façons d'écrire dans un fichier en Python](https://www.afternerd.com/blog/different-ways-to-create-file-in-python/)

Merci d'avoir lu cet article sur l'écriture d'un fichier texte en Python. N'hésitez pas à explorer davantage et à utiliser cette compétence pour vos projets futurs. Bon codage!