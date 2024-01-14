---
title:    "Elixir: Écrire un fichier texte"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Elixir

Écrire un fichier texte en utilisant le langage de programmation Elixir peut sembler être une tâche simple et banale, mais cela peut en fait être très utile. Les fichiers texte sont un format couramment utilisé pour stocker de l'information dans un format lisible par l'homme, ce qui en fait un bon moyen pour stocker des données ou des configurations.

## Comment faire

Pour écrire un fichier texte en Elixir, vous pouvez utiliser la fonction `File.write/2`. Cette fonction prend deux arguments: le nom du fichier à écrire et le contenu à écrire dans le fichier. Voici un exemple:

```Elixir
File.write("mon_fichier.txt", "Bonjour le monde!")
```
Ce code écrira "Bonjour le monde!" dans un fichier nommé "mon_fichier.txt". Vous pouvez également spécifier un chemin complet pour le fichier, par exemple `"chemin/vers/mon_fichier.txt"`. Si le fichier n'existe pas, il sera créé.

Pour écrire plusieurs lignes de texte, vous pouvez utiliser l'attribut `contenu` qui accepte un tableau de chaînes de caractères. Le code suivant écrira trois lignes de texte dans le même fichier:

```Elixir
contenu = ["Bonjour", "Comment ça va?", "Au revoir!"]
File.write("mon_fichier.txt", contenu)
```

## Plongée en profondeur

Lorsque vous écrivez un fichier texte en utilisant Elixir, vous devez prendre en compte différents paramètres tels que le codage, les permissions et le mode d'écriture. Vous pouvez spécifier ces paramètres à l'aide d'options supplémentaires dans la fonction `File.write/2`.

Par exemple, vous pouvez spécifier le mode d'écriture du fichier en utilisant l'option `:write` avec la valeur `:append` pour ajouter du contenu à la fin du fichier plutôt que de l'écraser. De plus, vous pouvez spécifier le codage en utilisant l'option `:encoding` avec la valeur `:utf8` pour s'assurer que le fichier est encodé correctement.

## Voir aussi

- Documentation sur la fonction `File.write/2` en [anglais](https://hexdocs.pm/elixir/File.html#write/2).
- Tutoriel sur la manipulation de fichiers en Elixir en [anglais](https://elixirschool.com/lessons/basics/files/).