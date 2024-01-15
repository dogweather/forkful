---
title:                "Création d'un fichier temporaire"
html_title:           "Gleam: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut sembler un concept étrange, mais c'est en fait une pratique très utile en programmation. Les fichiers temporaires sont utilisés pour stocker des données temporaires ou pour effectuer des opérations plus complexes sans alourdir le code principal.

## Comment le faire

Pour créer un fichier temporaire en utilisant Gleam, il existe une fonction intégrée appelée `File.tempfile/1`. Cela prend en paramètre une chaîne de caractères optionnelle qui sera utilisée comme préfixe pour le nom du fichier temporaire. Voici un exemple de code :

```Gleam
import File

File.tempfile("log_") |> handle_file
```

Cet exemple créera un fichier temporaire dont le nom commencera par "log_" et le passera à la fonction `handle_file` pour y effectuer des opérations supplémentaires.

De plus, il est important de noter que la fonction `File.tempfile/1` renvoie un type de données `Result<File.Error, File.Handle>`. Cela signifie qu'il peut y avoir des erreurs lors de la création du fichier temporaire, il est donc conseillé de les gérer correctement dans votre code.

## Plongée en profondeur

En utilisant la fonction `File.tempfile/1`, Gleam créera un fichier temporaire dans le répertoire de travail actuel. Cependant, il est également possible de spécifier un autre répertoire en utilisant la fonction `File.tempdir/1`. Cette fonction prend en paramètre une chaîne de caractères représentant le chemin du répertoire où le fichier temporaire sera créé.

Il est également important de noter que les fichiers temporaires créés par Gleam seront automatiquement supprimés une fois que le programme aura terminé son exécution. Cela garantit que votre système n'est pas encombré par des fichiers inutiles.

## Voir aussi

- Documentation officielle de la fonction `File.tempfile/1` : https://gleam.run/documentation/modules/io/file.html#tempfile
- Tutoriel Gleam sur la gestion des fichiers : https://gleam.run/book/tutorials/files.html