---
title:                "Ruby: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Écrire un fichier texte est une compétence essentielle pour tout programmeur de Ruby. Cela vous permet de stocker et d'organiser vos données d'une manière facilement lisible par les humains et les machines. De plus, il est crucial pour produire des résultats cohérents et reproductibles dans vos scripts.

## Comment faire
Pour écrire un fichier texte en Ruby, utilisez la méthode `File.write()`. Voici un exemple de code avec des données de fruits et de leurs quantités correspondantes :

```Ruby
fruits = ["pomme", "banane", "orange"]
quantites = [3, 5, 2]

File.write("fruits.txt", "Voici une liste de fruits avec leurs quantités : \n")
for i in 0...fruits.length
    File.write("fruits.txt", "#{fruits[i]} : #{quantites[i]} \n", mode: "a")
end
```

Le résultat de ce code sera un fichier texte nommé "fruits.txt" avec le contenu suivant :

```
Voici une liste de fruits avec leurs quantités :
pomme : 3
banane : 5
orange : 2
```

## Plongée en profondeur
La méthode `File.write()` prend trois arguments : le nom du fichier, le contenu à écrire et un mode optionnel. Le mode par défaut est "w" (write) qui crée un nouveau fichier ou écrase un fichier existant avec le même nom. En utilisant le mode "a" (append), vous pouvez ajouter du contenu à la fin d'un fichier existant sans effacer son contenu précédent.

De plus, vous pouvez utiliser des fonctions de formatage de texte pour rendre votre fichier plus lisible et organisé. Par exemple, en utilisant la méthode `sprintf()` avec le symbole "%.2f", vous pouvez limiter le nombre de décimales dans des nombres à virgule flottante dans votre fichier texte.

## Voir aussi
- La documentation officielle de Ruby sur `File.write()`: https://ruby-doc.org/core-3.0.1/File.html#method-c-write
- Un tutoriel sur l'écriture de fichiers en Ruby : https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-methods/cheatsheet
- Un guide pratique pour le formatage de texte en Ruby : https://nestacms.com/docs/creating-content/text-formatting-markdown-cheat-sheet