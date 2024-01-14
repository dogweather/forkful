---
title:                "Elixir: Vérifier l'existence d'un répertoire"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important pour tout programmeur d'être capable de vérifier l'existence d'un répertoire dans son code. Cela peut être utile pour vérifier si un fichier peut être créé dans un répertoire spécifique ou si un répertoire est accessible avant de manipuler des fichiers à l'intérieur.

## Comment faire

Pour vérifier si un répertoire existe en Elixir, nous pouvons utiliser la fonction `File.cwd?/1`. Cette fonction prend en paramètre un chemin de répertoire et renvoie `true` si le répertoire existe et `false` sinon. Voici un exemple de code:

```Elixir
if File.cwd?("chemin/vers/mon/répertoire") do
  IO.puts "Le répertoire existe!"
else
  IO.puts "Le répertoire n'existe pas!"
end
```

Si le répertoire existe, "Le répertoire existe!" sera imprimé dans la console. Sinon, c'est "Le répertoire n'existe pas!" qui sera affiché.

## Plongée en profondeur

En réalisant cette vérification, il est important de noter que le chemin de répertoire passé en paramètre doit être un chemin absolu et non relatif. De plus, la fonction renvoie `false` si le chemin de répertoire n'existe pas, mais aussi si le chemin mène à un fichier plutôt qu'à un répertoire.

## Voir aussi

- [Documentation Elixir pour la fonction `File.cwd?/1`](https://hexdocs.pm/elixir/File.html#cwd?/1)
- [Guide Elixir pour les opérations de fichiers et de répertoires](https://elixir-lang.org/getting-started/file-operations.html)