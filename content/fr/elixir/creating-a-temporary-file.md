---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Créer un fichier temporaire, c'est exactement ce que cela signifie : un fichier destiné à stocker des informations pour une courte période. Les programmeurs le font pour gérer les flux de travail qui nécessitent un stockage temporaire sans affecter les données permanentes.

## Comment faire :

Pour créer un fichier temporaire en Elixir, vous pouvez utiliser le module `:file` de Erlang/OTP. Voici comment :

1. Générez un nom de fichier unique et créez le fichier.

```elixir
{:ok, path} = :file.mktemp()
IO.puts(path)
```

2. Écrivez dans le fichier temporaire.

```elixir
{:ok, file} = File.open(path, [:write])
IO.binwrite(file, "Du texte temporaire")
File.close(file)
```

3. Lisez le contenu du fichier temporaire.

```elixir
{:ok, file} = File.read(path)
IO.puts(file)
```

## Plongée profonde

Au fil des années, de nombreux langages et systèmes ont fourni diverses manières de créer des fichiers temporaires. En Elixir, grâce à l'interopérabilité avec Erlang/OTP, nous pouvons profiter du module `:file`.

Il existe d'autres alternatives en Elixir pour créer des fichiers temporaires. Par exemple, vous pouvez utiliser le module `System` pour obtenir un nom de fichier unique basé sur le PID du processus Elixir, puis créer le fichier à l'aide de `File.open/2`.

Les détails de mise en œuvre comprennent l'utilisation de `:file.mktemp/0,1,2` qui crée un fichier dans le répertoire temporaire. Il garantit également que le nom de fichier est unique en utilisant le PID du processus Erlang actuel et un numéro unique.

## À voir également

Pour plus d'informations sur le traitement des fichiers en Elixir, consultez la documentation officielle de [File](https://hexdocs.pm/elixir/File.html) et [:file](http://erlang.org/doc/man/file.html). 

La question pertinente sur [Stack Overflow](https://stackoverflow.com/questions/35473854/how-to-generate-a-temporary-file-name-in-elixir) fournit également diverses idées et solutions pour la gestion des fichiers temporaires en Elixir.