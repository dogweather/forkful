---
title:                "Vérifier si un répertoire existe"
html_title:           "Elixir: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier si un répertoire existe consiste à utiliser un programme pour déterminer si un dossier spécifique existe sur un système de fichiers. Les programmeurs font cela pour éviter les erreurs lors de l'ouverture ou de la manipulation de fichiers dans un répertoire qui pourrait ne pas exister.

## Comment faire :
Voici comment vous pouvez vérifier si un répertoire existe en Elixir à l'aide de la fonction `File.dir?/1` :

```Elixir
if File.dir?("chemin/vers/le/répertoire") do
  IO.puts "Le répertoire existe."
else
  IO.puts "Le répertoire n'existe pas."
end
```

Cela affichera "Le répertoire existe." si le répertoire spécifié existe, et "Le répertoire n'existe pas." dans le cas contraire.

## Immersion dans le sujet :
La fonction `File.dir?/1` est implémentée en utilisant les appels système de bas niveau, ce qui la rend très rapide et efficace. Historiquement, la vérification de l'existence d'un répertoire était plus complexe dans les anciennes versions d'Elixir, mais elle a été simplifiée en version actuelle.
En alternative, vous pouvez aussi utiliser `File.ls/1` et attraper une exception `{:error, _}` si le dossier n'existe pas, mais cette méthode est généralement considérée comme moins idéale car elle implique un traitement coûteux des exceptions.

## Voir aussi :
1. Documentation Elixir pour `File.dir?/1`: [https://hexdocs.pm/elixir/File.html#dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
2. Discussion StackOverflow sur la vérification de l'existence d'un répertoire en Elixir : [https://stackoverflow.com/questions/21006221/how-to-check-if-a-directory-exists-in-elixir](https://stackoverflow.com/questions/21006221/how-to-check-if-a-directory-exists-in-elixir)