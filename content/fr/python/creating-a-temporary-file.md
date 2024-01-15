---
title:                "Créer un fichier temporaire"
html_title:           "Python: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Python ?

Créer un fichier temporaire en Python peut être utile lorsqu'on a besoin d'utiliser des données temporaires dans un programme. Cela permet notamment de stocker des informations de manière temporaire sans avoir à créer un nouveau fichier ou à écraser un fichier existant.

## Comment faire pour créer un fichier temporaire en Python ?

```Python
# Importer le module "tempfile"
import tempfile

# Créer un fichier temporaire en utilisant la méthode ".TemporaryFile()"
mon_fichier_temp = tempfile.TemporaryFile()

# Écrire du contenu dans le fichier temporaire
mon_fichier_temp.write(b"Voici un exemple de fichier temporaire en Python.")

# Déplacer le curseur au début du fichier
mon_fichier_temp.seek(0)

# Lire le contenu du fichier
contenu = mon_fichier_temp.read()

# Afficher le résultat
print(contenu)

# Fermer le fichier temporaire
mon_fichier_temp.close()
```

**Résultat :**

```
b'Voici un exemple de fichier temporaire en Python.'
```

## Plongeons plus en profondeur

Le module "tempfile" propose différentes méthodes pour créer des fichiers temporaires en Python, en fonction de nos besoins. Par exemple, la méthode ".NamedTemporaryFile()" permet de créer un fichier temporaire avec un nom spécifique, tandis que la méthode ".SpooledTemporaryFile()" permet de stocker le contenu en mémoire plutôt que dans un fichier physique sur le disque.

Il est également possible de spécifier le mode d'ouverture du fichier temporaire (lecture, écriture, ajout), ainsi que d'autres options telles que le préfixe et le suffixe du nom du fichier.

Pour en savoir plus sur les différentes possibilités offertes par le module "tempfile", vous pouvez consulter la documentation officielle : https://docs.python.org/fr/3/library/tempfile.html

## Voir aussi

- Documentation officielle sur le module "tempfile" en Python : https://docs.python.org/fr/3/library/tempfile.html
- Tutoriel sur la création de fichiers temporaires en Python : https://www.tutorialspoint.com/python/temporary_file.htm