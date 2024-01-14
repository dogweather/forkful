---
title:    "Ruby: Écrire un fichier texte"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi 

Écrire un fichier texte est une tâche courante en programmation, que ce soit pour stocker des données, générer un rapport ou créer une interface utilisateur simple. Dans cet article, nous allons explorer comment écrire un fichier texte en utilisant le langage de programmation Ruby.

## Comment faire

Il y a plusieurs façons d'écrire un fichier texte en Ruby, mais la méthode la plus simple consiste à utiliser la méthode `File.open`. Voici un exemple de code :

```Ruby
File.open("nouveau_fichier.txt", "w") do |fichier|
  fichier.puts "Ce texte sera écrit dans le fichier."
  fichier.puts "Vous pouvez également ajouter autant de lignes que vous le souhaitez."
end
```

Ce code crée un nouveau fichier appelé "nouveau_fichier.txt" et y écrit deux lignes de texte. La première ligne utilise la méthode `puts` pour ajouter le texte au fichier, tandis que la seconde ligne utilise la même méthode mais avec une syntaxe plus courte.

Une fois que vous avez exécuté ce code, vous devriez voir un nouveau fichier texte dans le même dossier que votre code Ruby, qui contient les deux lignes de texte que vous avez ajoutées. 

## Plongée en profondeur

Si vous souhaitez ajouter du contenu à un fichier existant plutôt que d'en créer un nouveau, vous pouvez utiliser la méthode `File.write`. Cette méthode prend deux arguments : le nom du fichier et le contenu à écrire. Voici un exemple de code :

```Ruby
File.write("mon_fichier.txt", "Ceci ajoute du contenu à mon fichier.")
```

Cette méthode écrira "Ceci ajoute du contenu à mon fichier." à la fin du contenu existant dans le fichier "mon_fichier.txt". Si vous voulez remplacer tout le contenu du fichier, vous pouvez utiliser la méthode `File.write` seule, sans spécifier le contenu existant.

## Voir aussi

- [La documentation Ruby](https://www.ruby-lang.org/fr/documentation/) 
- [Un tutoriel sur l'écriture de fichiers en Ruby](https://www.rubyguides.com/2018/08/writing-files-in-ruby/)
- [La page "File" de Ruby-Doc](https://ruby-doc.org/core-2.6.3/File.html)