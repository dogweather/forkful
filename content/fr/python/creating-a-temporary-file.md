---
title:                "Python: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire? 

Nous avons tous été confrontés à cette situation : vous avez besoin de stocker temporairement des données ou de manipuler un grand nombre de fichiers, mais vous ne voulez pas encombrer votre système de fichiers principal avec des éléments inutiles. C'est là que les fichiers temporaires entrent en jeu! En créant un fichier temporaire, vous pouvez facilement stocker des données de manière temporaire et effacer automatiquement le fichier une fois que vous avez terminé son utilisation.

## Comment créer un fichier temporaire en Python?

La création d'un fichier temporaire en Python est simple et peut être faite en utilisant le module `tempfile`. Voici un exemple de code pour créer un fichier temporaire et y écrire du contenu : 

```Python
import tempfile

# Créer un fichier temporaire
with tempfile.NamedTemporaryFile() as file:
    # Écrire du contenu dans le fichier
    file.write(b"Ceci est un exemple de contenu temporaire")

    # Afficher le chemin du fichier temporaire
    print("Le chemin du fichier temporaire est :", file.name)

# Le fichier temporaire sera automatiquement effacé après la sortie de la boucle `with`
```

Output : 
```
Le chemin du fichier temporaire est : /var/folders/7y/q3r121yx69l_c1yf9350xzlw0000gn/T/tmpmv3r_i7m
```

Il est également possible de spécifier une extension de fichier pour votre fichier temporaire en utilisant le paramètre `suffix`. Par exemple :

```Python
# Créer un fichier temporaire avec une extension ".txt"
with tempfile.NamedTemporaryFile(suffix=".txt") as file:
    # Écrire du contenu dans le fichier
    file.write(b"Ceci est un exemple de contenu temporaire")

    # Afficher le chemin du fichier temporaire
    print("Le chemin du fichier temporaire est :", file.name)

# Le fichier temporaire ".txt" sera automatiquement effacé après la sortie de la boucle `with`
```

## Plongée en profondeur sur la création de fichiers temporaires

Le module `tempfile` offre plusieurs options pour créer et manipuler des fichiers temporaires, telles que `Tempfile()`, `TemporaryDirectory()`, `SpooledTemporaryFile()` et bien d'autres encore. Il est également possible de modifier les options de sauvegarde par défaut, telles que le répertoire dans lequel les fichiers temporaires seront stockés ou le préfixe utilisé pour nommer les fichiers.

Pour en savoir plus sur tous les détails et les options disponibles, consultez la [documentation officielle](https://docs.python.org/fr/3/library/tempfile.html) pour le module `tempfile`.

## Voir aussi

- [Documentation officielle sur le module `tempfile`](https://docs.python.org/fr/3/library/tempfile.html)
- [Tutoriel vidéo sur les fichiers temporaires en Python](https://www.youtube.com/watch?v=fgsg1EsEirg)
- [Article sur les modules en Python pour la manipulation de fichiers temporaires](https://realpython.com/python-tempfile/)