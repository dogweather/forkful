---
title:                "Vérifier si un répertoire existe"
html_title:           "Ruby: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Vérifier si un répertoire existe est une tâche courante en programmation. Cela permet de s'assurer qu'un répertoire spécifique existe avant d'y accéder ou d'y effectuer des opérations. Cela peut également aider à éviter les erreurs et les bogues dans le code.

## Comment faire:
Vérifier si un répertoire existe en Ruby est assez simple. Il suffit d'utiliser la méthode ```Dir.exist? ``` avec le nom du répertoire en tant que paramètre. Voici un exemple de code:

```Ruby
if Dir.exist?("mon_repertoire")
    puts "Le répertoire existe!"
else
    puts "Le répertoire n'existe pas."
end
```

Et voici un exemple de résultat si le répertoire existe réellement:

```
Le répertoire existe!
```

## Plongée Profonde:
La vérification de l'existence d'un répertoire est une pratique courante dans la programmation depuis longtemps. Avant, cela se faisait souvent en utilisant la méthode ```File.exist? ```, mais ```Dir.exist? ``` est maintenant la méthode recommandée pour vérifier l'existence d'un répertoire spécifique.

Il existe également d'autres alternatives pour vérifier si un répertoire existe, telles que la méthode ```File.directory? ``` et l'utilisation de chemins absolus ou relatifs pour accéder au répertoire.

En termes d'implémentation, la méthode ```Dir.exist? ``` utilise la fonction système ```stat ``` pour vérifier si le répertoire existe. Elle renvoie un booléen (true ou false) en fonction de l'existence du répertoire.

## Voir aussi:
- [Documentation officielle de Ruby sur la méthode Dir.exist?](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
- [Documentation officielle de Ruby sur la méthode File.exist?](https://ruby-doc.org/core-3.0.0/File.html#method-c-exist-3F)
- [Un tutoriel sur la manipulation des répertoires en Ruby](https://www.rubyguides.com/2018/07/ruby-file-directory/)