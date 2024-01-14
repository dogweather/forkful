---
title:                "Elixir: Création d'un fichier temporaire"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut sembler anodin, mais c'est en fait une pratique très utile lorsque vous travaillez avec des données temporaires dans vos applications Elixir. Que ce soit pour stocker des données temporaires utilisées par votre application ou pour effectuer des tests, créer un fichier temporaire peut vous éviter bien des tracas.

## Comment faire

Il existe plusieurs façons de créer un fichier temporaire en utilisant Elixir. Voici deux exemples différents avec leur sortie respective :

```elixir
# Créer un fichier temporaire vide avec la bibliothèque standard File
temp_file = File.tempfile()

# Imprimer le chemin vers le fichier temporaire
IO.inspect(temp_file.path)

# Créer un fichier temporaire avec du contenu en utilisant la bibliothèque standard Tempfile
content = "Ceci est un exemple de contenu pour notre fichier temporaire."
temp_file2 = Tempfile.write!(content)

# Lire le contenu du fichier temporaire
IO.inspect(File.read!(temp_file2.path))
```

Sortie :

```elixir
"/var/folders/yx/_58t5c313g5cjr7xsq4z09t80000gn/T/elixir20170330-24343-1w2wvq"
"Ceci est un exemple de contenu pour notre fichier temporaire."
```

Comme vous pouvez le voir, vous pouvez utiliser les modules standard `File` ou `Tempfile` pour créer des fichiers temporaires et interagir avec eux. Ces modules rendent le processus très simple et vous permettent de gérer facilement les fichiers temporaires que vous avez créés.

## Plongée en profondeur

Créer un fichier temporaire en utilisant la bibliothèque standard `File` est une option très simple, mais il existe également des bibliothèques tierces qui peuvent offrir des fonctionnalités supplémentaires. Par exemple, la bibliothèque `Tempfile` vous permet de spécifier un préfixe pour vos fichiers temporaires, de les créer dans un répertoire spécifique ou même de les supprimer automatiquement après une certaine période de temps.

Il est également important de noter que les fichiers temporaires ne sont pas automatiquement supprimés une fois que votre application a terminé de les utiliser. Vous devez donc prendre soin de les supprimer manuellement une fois qu'ils ne sont plus nécessaires. Vous pouvez le faire en utilisant la fonction `File.rm/1` ou en utilisant la bibliothèque `Tempfile` pour supprimer automatiquement les fichiers après un certain temps.

## Voir aussi

- [Documentation officielle de la bibliothèque Tempfile](https://hexdocs.pm/tempfile/Tempfile.html)
- [Documentation officielle de la bibliothèque File](https://hexdocs.pm/elixir/File.html)
- [Article sur les fichiers temporaires en Elixir](https://medium.com/@joejamesmetcalf/working-with-temporary-files-in-elixir-bc2a1e40c563)