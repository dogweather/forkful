---
title:    "Elixir: Lecture d'un fichier texte"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Pourquoi

Lire et manipuler des fichiers texte est une compétence essentielle pour tout programmeur Elixir. Cela permet de lire les données externes et de les utiliser dans votre code, ce qui peut être très utile dans de nombreux cas.

# Comment faire

Pour lire un fichier texte en Elixir, nous pouvons utiliser la fonction `File.read!/1` en spécifiant le chemin du fichier en tant que paramètre. Cela renvoie le contenu du fichier sous forme de chaîne de caractères.

```Elixir
# Lecture du fichier
data = File.read!("chemin/vers/votre/fichier.txt")

# Impression du contenu
IO.puts(data)
```

Dans l'exemple ci-dessus, la variable `data` contiendra le contenu du fichier texte, et avec la fonction `IO.puts/1` nous pouvons l'imprimer dans la console.

# Plongée en profondeur

En utilisant `File.read!/1`, nous pouvons également spécifier des options supplémentaires pour personnaliser la lecture d'un fichier. Par exemple, nous pouvons spécifier le nombre de bytes à lire avec l'option `:read_bytes`, ou spécifier un délimiteur avec l'option `:line, line_break: "\n"`.

De plus, pour une meilleure gestion des erreurs, nous pouvons utiliser la fonction `File.read/1` qui renvoie un tuple avec le contenu du fichier et un code d'erreur en cas de problème de lecture.

# Voir aussi

- [Documentation sur la lecture de fichier en Elixir](https://hexdocs.pm/elixir/File.html#read!/1)
- [Article sur les opérations de fichier en Elixir](https://medium.com/@kyrylo/how-to-perform-file-operations-in-elixir-9e576adb5d70)