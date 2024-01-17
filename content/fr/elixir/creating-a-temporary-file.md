---
title:                "Créer un fichier temporaire"
html_title:           "Elixir: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Créer un fichier temporaire est une pratique courante en programmation qui consiste à créer un fichier qui ne restera que temporairement sur le système. Les programmeurs utilisent souvent cette méthode pour stocker temporairement des données ou des informations pendant l'exécution d'un programme.

## Comment faire:
Pour créer un fichier temporaire en utilisant Elixir, vous pouvez utiliser la fonction `Tempfile` du module `File`. Voici un exemple de code:

```Elixir
{:ok, file} = File.tempfile("my_temp_file")
IO.puts file.path
```

Dans cet exemple, nous utilisons la fonction `tempfile` pour créer un fichier avec le préfixe "my_temp_file" dans le répertoire de travail actuel. La fonction retourne un tuple contenant l'atome `:ok` et l'objet de type `File`. Nous pouvons ensuite utiliser la méthode `path` pour obtenir le chemin du fichier temporaire créé.

## Plongée en profondeur:
La création de fichiers temporaires est une pratique qui a été largement utilisée depuis les débuts de la programmation. Avant l'arrivée des ordinateurs à disque dur, les fichiers temporaires étaient créés en utilisant des bandes magnétiques ou des cartes perforées. De nos jours, il existe plusieurs alternatives pour créer des fichiers temporaires, notamment en utilisant la mémoire vive (RAM) ou en utilisant des bases de données.

En ce qui concerne Elixir, la fonction `tempfile` utilise en fait la mémoire RAM pour stocker temporairement le fichier avant de l'écrire sur le disque dur. Cela le rend plus rapide que les autres méthodes de création de fichiers temporaires.

## À voir aussi:
- [Documentation sur `File` dans Elixir](https://hexdocs.pm/elixir/File.html#tempfile/2)
- [Plus d'informations sur la création de fichiers temporaires en programmation](https://www.baeldung.com/java-temporary-files)
- [Article sur les avantages et les inconvénients de différentes méthodes de création de fichiers temporaires](https://unix.stackexchange.com/questions/31991/temporary-file-for-shell-script)