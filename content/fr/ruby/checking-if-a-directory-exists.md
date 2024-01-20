---
title:                "Vérifier si un répertoire existe"
html_title:           "Arduino: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Contrôler l'existence d'un répertoire est une pratique pour s'assurer qu'un dossier particulier est présent à l'endroit attendu. Les programmeurs font cela pour prévenir les erreurs qui pourraient survenir lorsqu'on tente d'accéder ou de manipuler un répertoire inexistant.

## Comment faire :

Voici comment vérifier l'existence d'un répertoire en Ruby :
```Ruby
if Dir.exist?("MonDossier")
  puts "Le dossier existe!"
else
  puts "Le dossier n'existe pas!"
end
```
Si "MonDossier" existe, alors votre console affichera "Le dossier existe!". Sinon, vous verrez "Le dossier n'existe pas!".

## Une analyse approfondie :

Historiquement, plusieurs méthodes ont été utilisées pour vérifier l'existence d'un répertoire en Ruby, comme la méthode `File.directory?`. Cependant, depuis la version Ruby 1.9, `Dir.exist?` est devenu la méthode préférée en raison de sa plus grande simplicité et clarté.

Il existe également des alternatives comme `Pathname.exist?` qui peut être utilisée pour vérifier l'existence d'une route d'accès que ce soit pour un répertoire ou un fichier.

Les détails d'implémentation de `Dir.exist?` sont assez simples : Ruby essaie d'ouvrir le dossier spécifié et renvoie `true` si cela réussit. Sinon, il renvoie `false`.

## Voir aussi :

- Documentation Ruby pour `Dir` : https://ruby-doc.org/core-2.7.0/Dir.html
- Guide sur les bases de navigation de fichiers et de répertoire avec Ruby: https://www.tutorialspoint.com/ruby/ruby_directories.htm
- Comparaison des différentes méthodes de vérification de l'existence d'un répertoire : https://stackoverflow.com/questions/2521053/how-to-check-if-a-directory-exists-in-ruby