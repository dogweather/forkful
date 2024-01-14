---
title:    "Python: Créer un fichier temporaire"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Pourquoi

Créer un fichier temporaire peut être utile lorsque vous travaillez avec des données sensibles ou confidentielles. Il peut également être utile pour stocker temporairement des données en cours de traitement, sans les écraser sur des fichiers existants.

##Comment faire

Il existe plusieurs méthodes pour créer un fichier temporaire en Python. La façon la plus simple est d'utiliser la bibliothèque standard `tempfile` en important le module dans votre code:

```Python
from tempfile import NamedTemporaryFile

# Crée un fichier temporaire appelé "exemple.txt"
temp_file = NamedTemporaryFile(prefix="exemple", suffix=".txt", delete=False)

# Écrit du contenu dans le fichier temporaire
temp_file.write("Ceci est un exemple de texte dans le fichier temporaire")

# Ferme le fichier temporaire
temp_file.close()
```

Vous pouvez également spécifier le chemin où vous souhaitez que le fichier temporaire soit créé en utilisant le paramètre `dir`:

```Python
# Crée un fichier temporaire dans le dossier "temp" de votre répertoire actuel
temp_file = NamedTemporaryFile(prefix="exemple", suffix=".txt", dir="temp", delete=False)
```

Si vous souhaitez créer un fichier temporaire vide, vous pouvez utiliser la méthode `tempfile.mkstemp()` qui retourne un tuple avec le descripteur de fichier et le chemin du fichier temporaire:

```Python
import tempfile

# Crée un fichier temporaire vide
temp_file, path = tempfile.mkstemp()
```

Pour supprimer automatiquement le fichier temporaire une fois qu'il n'est plus nécessaire, vous pouvez utiliser la méthode `tempfile.TemporaryFile()`:

```Python
import tempfile

# Crée un fichier temporaire qui sera automatiquement supprimé une fois fermé
with tempfile.TemporaryFile() as temp:
    # Effectue des opérations sur le fichier temporaire
```

##Plongée en profondeur

Lorsque vous créez un fichier temporaire en utilisant la bibliothèque `tempfile`, le fichier est créé avec un nom aléatoire pour éviter les conflits de noms de fichiers. Vous pouvez également spécifier un préfixe ou un suffixe pour le nom du fichier temporaire en le passant en tant qu'argument dans `NamedTemporaryFile()` ou en utilisant le paramètre `prefix` dans `mkstemp()`.

Il existe également d'autres bibliothèques externes qui peuvent être utiles lorsque vous travaillez avec des fichiers temporaires en Python, telles que `tempdir` et `tempfiles`.

##Voir aussi

- Documentation officielle de la bibliothèque `tempfile`: https://docs.python.org/fr/3/library/tempfile.html
- Tutoriel sur la gestion des fichiers en Python: https://www.tutorialspoint.com/python/python_files_io.htm
- Utilisation des fichiers temporaires en Python: https://www.geeksforgeeks.org/temporary-files-python/