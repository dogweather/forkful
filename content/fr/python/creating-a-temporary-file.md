---
title:    "Python: Création d'un fichier temporaire"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Python ?

Créer un fichier temporaire peut être utile lorsque l'on manipule des données temporaires ou sensibles dans un programme Python. Cela permet de stocker temporairement ces données de manière sécurisée et de les supprimer une fois qu'elles ne sont plus nécessaires, évitant ainsi de surcharger le système avec des fichiers inutiles.

## Comment créer un fichier temporaire en Python ?

La création d'un fichier temporaire en Python est assez simple. Il suffit d'utiliser le module `tempfile` et d'appeler la méthode `NamedTemporaryFile()`. Voici un exemple de code :

```Python
import tempfile

with tempfile.NamedTemporaryFile() as temp:
  temp.write(b"Hello world!")

print("Le fichier temporaire a été créé avec succès.")
```

Le code ci-dessus crée un fichier temporaire avec le contenu "Hello world!" et imprime un message de confirmation. En utilisant le gestionnaire de contexte `with`, le fichier temporaire sera automatiquement supprimé une fois que le bloc de code sera terminé.

## Plonger plus en profondeur

Il est possible de spécifier certaines options lors de la création d'un fichier temporaire, telles que le mode d'ouverture, le préfixe et le suffixe du nom de fichier, etc. Vous pouvez également utiliser la méthode `TemporaryFile()` pour créer un fichier sans l'utiliser en tant que gestionnaire de contexte.

Il est également important de noter que le fichier temporaire créé sera automatiquement supprimé à la fin du programme. Cependant, si vous souhaitez le supprimer manuellement à un moment précis, vous pouvez utiliser la méthode `delete()`.

## Voir aussi

- [Documentation du module tempfile en français](https://docs.python.org/fr/3/library/tempfile.html)
- [Article sur les fichiers temporaires en Python](https://realpython.com/python-tempfile/) de Real Python.