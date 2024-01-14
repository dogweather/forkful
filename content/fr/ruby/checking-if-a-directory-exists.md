---
title:                "Ruby: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vérifier si un répertoire existe est une tâche importante en programmation. Cela peut être utile lorsque vous créez une application qui doit manipuler et gérer des fichiers ou lorsque vous devez vous assurer qu'un certain chemin de fichier existe avant d'y accéder. Dans cet article, nous allons plonger dans le processus de vérification de l'existence d'un répertoire en utilisant Ruby.

## Comment faire

Pour vérifier si un répertoire existe en utilisant Ruby, nous allons utiliser la méthode `File.exist?` et lui passer en argument le chemin du répertoire que nous voulons vérifier. Voici un exemple de code avec une sortie possible :

```ruby
if File.exist?("documents/articles")
  puts "Le répertoire 'articles' existe."
else
  puts "Le répertoire n'existe pas."
end
```

Sortie :

```
Le répertoire 'articles' existe.
```

Si vous voulez également vérifier si un répertoire spécifique existe dans un chemin de fichier plus complexe, vous pouvez combiner `File.exist?` avec la méthode `Dir.glob`. Par exemple :

```ruby
if Dir.glob("documents/*/articles").empty?
  puts "Le répertoire 'articles' n'existe pas."
end
```

Sortie :

```
Le répertoire 'articles' n'existe pas.
```

## Plongée en profondeur

Maintenant, si vous voulez vous plonger un peu plus en détail dans la vérification de l'existence d'un répertoire en utilisant Ruby, voici quelques éléments à prendre en compte :

- La méthode `File.exist?` renverra `true` si le chemin donné correspond à un répertoire ou un fichier existant, et `false` sinon.
- Si vous voulez vérifier si un fichier spécifique existe, vous pouvez utiliser la méthode `File.exist?` avec le nom complet du fichier au lieu du répertoire.
- Si vous voulez créer un répertoire s'il n'existe pas, vous pouvez utiliser la méthode `Dir.mkdir`.
- Si vous voulez continuer à manipuler le répertoire après avoir vérifié son existence, vous pouvez utiliser la classe `Dir` et ses méthodes pour traverser le répertoire et effectuer des opérations sur ses fichiers.

## Voir aussi

Pour plus d'informations sur la manipulation des fichiers et des répertoires en utilisant Ruby, vous pouvez consulter ces liens :

- Documentation officielle de Ruby sur la classe `Dir`: https://ruby-doc.org/core-2.7.0/Dir.html
- Tutoriel sur la gestion des fichiers et des répertoires en Ruby: https://www.rubyguides.com/2018/10/working-with-files-ruby/
- Tutoriel sur la vérification de l'existence d'un répertoire avec Ruby: https://www.digitalocean.com/community/tutorials/how-to-use-the-ruby-file-class