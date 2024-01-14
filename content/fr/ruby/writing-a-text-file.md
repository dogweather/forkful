---
title:                "Ruby: Écrire un fichier texte"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en Ruby?

Pour les écrivains de code passionnés, il est important de comprendre comment utiliser différentes méthodes pour stocker et manipuler des données. Dans le monde de la programmation Ruby, l'écriture de fichiers texte est une compétence essentielle à maîtriser.

## Comment écrire un fichier texte en Ruby?

Pour écrire un fichier texte en Ruby, vous pouvez utiliser la méthode `File.write()` et spécifier le nom du fichier ainsi que les données que vous souhaitez écrire. Voici un exemple de code qui écrit une phrase simple dans un fichier texte et l'imprime sur la console:

```Ruby
# Création et écriture d'un fichier texte
File.write("mon_fichier.txt", "Ceci est un exemple de phrase écrite dans un fichier texte en Ruby!")

# Lecture du fichier et impression sur la console
puts File.read("mon_fichier.txt")
```

Lorsque vous exécutez ce code, vous verrez la phrase "Ceci est un exemple de phrase écrite dans un fichier texte en Ruby!" imprimée sur votre console.

## Plongeon en profondeur

Il existe de nombreuses façons d'écrire et de manipuler des fichiers texte en Ruby. Pour une meilleure compréhension, vous pouvez explorer les méthodes `File.open()` et `File.write()` ainsi que les différents modes tels que "r" pour lecture, "w" pour écriture et "a" pour ajout.

Vous pouvez également en apprendre davantage sur les différentes fonctionnalités de Ruby pour travailler avec des fichiers, comme les options de formatage et de manipulation des données.

# Voir aussi

Pour plus d'informations sur l'écriture de fichiers texte en Ruby, consultez les liens ci-dessous:
- [Documentation officielle de Ruby sur la classe File] (https://ruby-doc.org/core-2.7.1/File.html)
- [Tutoriel W3Schools sur les fichiers en Ruby] (https://www.w3schools.com/ruby/ruby_file_opens_create.asp)
- [Guide de programmation en Ruby de cours OpenClassrooms] (https://openclassrooms.com/fr/courses/1861441-pratiquer-la-programmation-avec-ruby/1861842-lire-et-ecrire-des-fichiers)

Maintenant que vous avez appris comment écrire un fichier texte en Ruby, vous pouvez aller de l'avant et utiliser cette compétence dans vos projets de programmation! Bonne chance!