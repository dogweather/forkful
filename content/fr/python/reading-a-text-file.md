---
title:                "Python: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

Il est important de savoir comment lire un fichier texte en Python car cela peut être utile dans de nombreuses situations, comme la manipulation de données ou la génération de rapports en utilisant des données stockées dans un fichier texte.

# Comment le faire

```Python
# Ouvrir le fichier en mode "lecture" en utilisant la fonction open()
fichier = open('mon_fichier.txt', 'r')

# Utiliser la méthode readlines() pour lire toutes les lignes du fichier et les stocker dans une liste
lignes = fichier.readlines()

# Parcourir la liste de lignes et les afficher
for ligne in lignes:
    print(ligne)

# Fermer le fichier
fichier.close()
```

# Plongez plus en profondeur

Il existe plusieurs méthodes pour lire un fichier texte en Python, telles que read(), readline(), et read(size). Chacune a ses propres spécificités et peut être utilisée en fonction des besoins spécifiques de votre code. Vous pouvez également spécifier l'encodage du fichier lors de l'ouverture en utilisant l'argument "encoding" de la fonction open().

# Voir aussi

- Documentation officielle de Python sur la lecture de fichiers: https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutoriel sur la lecture de fichiers avec Python: https://www.digitalocean.com/community/tutorials/how-to-handle-plain-text-files-in-python-3
- Liste de toutes les méthodes de lecture de fichiers en Python: https://docs.python.org/fr/3/library/io.html#text-io-objects