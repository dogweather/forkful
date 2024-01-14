---
title:    "Python: Écrire un fichier texte"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est une compétence essentielle pour tout programmeur Python. Les fichiers texte sont une façon pratique de stocker et de manipuler de grandes quantités de données de manière organisée. Dans cet article, nous allons discuter de l'importance d'écrire un fichier texte et de la façon de le faire efficacement en Python.

## Comment faire

Pour écrire un fichier texte en Python, nous allons utiliser la fonction `open()` qui ouvre un fichier et retourne un objet de type fichier. Nous devrons également spécifier le mode d'ouverture du fichier, qui peut être "w" pour l'écriture ou "a" pour l'ajout. Ensuite, nous pouvons utiliser la méthode `write()` pour écrire du contenu dans le fichier. Voici un exemple de code :

```python
# Ouverture du fichier en mode écriture
with open("mon_fichier.txt", "w") as f:
    # Écriture de texte dans le fichier
    f.write("Bonjour tout le monde !\n")
    f.write("Je suis un fichier texte écrit en Python.\n")
    f.write("C'est si facile !")
```

L'utilisation de la clause `with` garantit que le fichier sera fermé automatiquement après avoir été utilisé. Voici le contenu du fichier "mon_fichier.txt" après l'exécution du code ci-dessus :

```
Bonjour tout le monde !
Je suis un fichier texte écrit en Python.
C'est si facile !
```

## Plongée en profondeur

En plus de l'écriture du contenu, nous pouvons également spécifier le codage du fichier en utilisant l'argument `encoding` de la fonction `open()`. Cela est utile lorsque l'on travaille avec des caractères spéciaux ou des langues étrangères. De plus, nous pouvons utiliser la méthode `writelines()` pour écrire une liste de chaînes de caractères dans le fichier. Enfin, il est important de toujours fermer le fichier après avoir fini de l'utiliser en utilisant la méthode `close()`. Voici un exemple complet :

```python
# Ouverture du fichier en mode écriture avec un codage spécifique
with open("mon_fichier.txt", "w", encoding="utf-8") as f:
    # Écriture d'une liste de chaînes de caractères dans le fichier
    lines = ["Première ligne.\n", "Deuxième ligne.\n", "Troisième ligne.\n"]
    f.writelines(lines)

# Fermeture du fichier
f.close()
```

Le contenu du fichier "mon_fichier.txt" est maintenant :

```
Première ligne.
Deuxième ligne.
Troisième ligne.
```

## Voir aussi

- [Documentation officielle Python pour les fichiers](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel sur la manipulation de fichiers en Python](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)
- [Chaîne Youtube : Programmer en Python](https://www.youtube.com/playlist?list=PLMS9Cy4Enq5JmIZtKE5OHJCI3jZfpASbR)