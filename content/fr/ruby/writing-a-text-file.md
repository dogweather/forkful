---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Écrire dans un fichier texte, c'est enregistrer des données ou du texte dans un fichier sur votre disque. On le fait pour la persistance des données, pour le partage, ou pour des logs.

## How to: (Comment faire :)
```Ruby
# Ouvrir et écrire dans un fichier
File.open('example.txt', 'w') do |file|
  file.puts "Salut Rubyistes!" 
end

# Ajouter du texte à un fichier existant
File.open('example.txt', 'a') do |file|
  file.puts "Une autre ligne!"
end

# Lire le fichier
puts File.read('example.txt')
```
```
Salut Rubyistes!
Une autre ligne!
```

## Deep Dive (Plongée en Profondeur)
Historiquement, écrire dans un fichier était plus complexe. Aujourd'hui, Ruby simplifie ce processus avec des méthodes comme `File.open`. Il existe des alternatives, comme `IO.write`, ou travailler directement avec des flux (`IO` objects). Niveau implémentation, Ruby gère la plupart des détails du système de fichiers sous-jacent, vous n'avez donc pas à vous en préoccuper.

## See Also (Voir Aussi)
- Ruby Doc on File: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Ruby Doc on IO: [ruby-doc.org/core/IO.html](https://ruby-doc.org/core/IO.html)
