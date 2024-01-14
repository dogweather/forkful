---
title:                "Elixir: Lecture d'un fichier texte"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Les fichiers texte sont un élément essentiel dans le développement de logiciels. Ils stockent des informations précieuses qui peuvent être utilisées pour plusieurs usages, tels que la configuration ou le stockage de données. Dans cet article, nous allons explorer comment lire un fichier texte en utilisant le langage de programmation Elixir. Que vous soyez un développeur chevronné ou que vous commenciez tout juste à apprendre le langage, la lecture de fichiers texte est une compétence essentielle à maîtriser.

## Comment faire

Pour lire un fichier texte en utilisant Elixir, nous avons besoin d'utiliser la fonction `File.read!` qui fait partie de la bibliothèque standard. Cette fonction prend en paramètre le chemin du fichier et retourne le contenu du fichier sous forme de chaîne de caractères. Voyons un exemple concret.

```
Elixir
content = File.read!("file.txt")
IO.puts(content)
```

Dans cet exemple, nous utilisons la fonction `IO.puts` pour afficher le contenu du fichier sur la console. Mais nous pouvons également utiliser les données à d'autres fins, comme les stocker dans une variable ou les traiter davantage.

Vous pouvez également spécifier le mode d'ouverture du fichier en utilisant le paramètre `mode` dans la fonction `File.read!`. Par exemple, si vous souhaitez lire un fichier uniquement en lecture, vous pouvez utiliser `File.read!("file.txt", [:raw])`.

## Plongée en profondeur

Maintenant que nous savons comment lire un fichier texte en utilisant Elixir, il est important de comprendre comment cela fonctionne réellement en interne. Lorsque nous utilisons la fonction `File.read!`, Elixir effectue les opérations suivantes :

1. Ouvre le fichier en utilisant la bibliothèque de fichiers du système d'exploitation.
2. Lit le contenu du fichier dans un tampon en mémoire.
3. Ferme le fichier.

Elixir rend ce processus très efficace en utilisant des opérations de lecture de fichiers asynchrones, ce qui signifie qu'il peut lire plusieurs fichiers à la fois sans bloquer l'exécution de notre code.

## Voir aussi

- [La documentation officielle sur la fonction `File.read!`](https://hexdocs.pm/elixir/File.html#read!/2)
- [Un article sur la lecture de fichiers en utilisant Elixir](https://medium.com/elixir-mastery/read-files-elixir-with-file-module-2ab6953a852e)
- [Un tutoriel vidéo sur la lecture de fichiers en utilisant Elixir](https://www.youtube.com/watch?v=iiqTwz5by8A)