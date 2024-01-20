---
title:                "Écriture vers l'erreur standard"
html_title:           "Ruby: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Ecrire vers une sortie d'erreur standard, c'est simplement envoyer des messages d'erreur ou de débogage vers un canal spécifique plutôt que vers la sortie standard. Les programmeurs le font pour garder leur code organisé et facile à lire, en séparant les messages d'erreur des autres sorties.

## Comment faire :
Voici un exemple de code Ruby montrant comment écrire vers la sortie d'erreur standard :
```Ruby 
$stderr.puts "Ceci est un message d'erreur"
```
Résultat :
```Ruby
Ceci est un message d'erreur
```

## Plongée en profondeur :
L'écriture vers la sortie d'erreur standard est une pratique courante en programmation et peut être utile dans de nombreuses situations. Elle permet de mieux gérer les erreurs et les messages de débogage, en les séparant du reste du code. Une alternative à l'écriture vers la sortie d'erreur standard est d'utiliser des outils de débogage spécifiques tels que Pry ou Byebug. Pour implémenter l'écriture vers la sortie d'erreur standard dans un programme Ruby, il suffit d'utiliser la méthode $stderr.puts().

## Voir aussi :
- [Documentation officielle de Ruby sur la sortie d'erreur standard](https://ruby-doc.org/core-3.0.0/IO.html#class-IO-label-Standard+Streams)