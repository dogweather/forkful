---
title:                "Python: Création d'un fichier temporaire"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut être très utile dans plusieurs situations en programmation Python. Cela peut être nécessaire pour stocker des données intermédiaires ou pour manipuler des fichiers sans risque de les écraser accidentellement. Dans cet article, nous allons découvrir comment créer un fichier temporaire en utilisant le module intégré `tempfile` de Python.

## Comment faire

Pour créer un fichier temporaire en Python, nous allons utiliser la fonction `mkstemp()` du module `tempfile`.

```Python
import tempfile

# Création du fichier temporaire
fichier_temporaire = tempfile.mkstemp()

# Affichage du chemin du fichier temporaire
print("Chemin du fichier temporaire :", fichier_temporaire[1])

# Écriture de données dans le fichier temporaire
with open(fichier_temporaire[1], "w") as f:
    f.write("Ceci est une donnée temporaire.")

# Lecture des données depuis le fichier temporaire
with open(fichier_temporaire[1], "r") as f:
    print("Données dans le fichier temporaire :", f.read())

# Suppression du fichier temporaire
tempfile.cleanup()
```
```
Output:
Chemin du fichier temporaire: /var/folders/q7/x8pcr76d2td6_5m0ngrl14000000gn/T/tmp5k1zuia4
Données dans le fichier temporaire: Ceci est une donnée temporaire.
```

Comme on peut le voir dans l'exemple ci-dessus, la fonction `mkstemp()` renvoie un tuple contenant deux valeurs : un handle pour le fichier temporaire et son chemin d'accès. Nous pouvons ensuite utiliser ce chemin d'accès pour lire ou écrire des données dans le fichier temporaire. Une fois que nous avons terminé de manipuler le fichier temporaire, nous pouvons le supprimer en appelant la fonction `cleanup()` du module `tempfile`.

## Plongée en profondeur

Le module `tempfile` de Python offre également d'autres fonctions pour créer des fichiers temporaires en spécifiant différents formats de noms ou en créant des dossiers temporaires. De plus, le paramètre `delete=False` peut être ajouté à la fonction `mkstemp()` pour éviter la suppression automatique du fichier temporaire lors de l'appel de la fonction `cleanup()`. Pour en savoir plus sur toutes les options disponibles, vous pouvez consulter la documentation officielle de Python sur le module `tempfile`.

## Voir aussi

- [La documentation officielle de Python sur le module `tempfile`](https://docs.python.org/fr/3/library/tempfile.html) 
- [Un exemple pratique de création de fichiers temporaires en Python](https://www.geeksforgeeks.org/temporary-file-creation-using-python/)