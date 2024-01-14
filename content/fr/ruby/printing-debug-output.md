---
title:                "Ruby: Impression de sortie de débogage"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage est un outil utile pour les programmeurs de Ruby pour identifier et résoudre les problèmes dans leur code. Cela permet de visualiser les valeurs des variables à différents points du programme et de comprendre comment le code s'exécute.

## Comment faire

Pour imprimer une sortie de débogage en Ruby, utilisez la méthode "puts" avec le message que vous souhaitez afficher. Vous pouvez également inclure des variables en utilisant l'opérateur de concaténation "+" pour les afficher avec le message.

```Ruby
# Exemple de code pour imprimer une sortie de débogage
age = 25
puts "L'âge est: " + age.to_s
```

Cela affichera la phrase "L'âge est: 25" dans votre terminal.

## Plongée en profondeur

Il existe également d'autres méthodes pour imprimer une sortie de débogage, telles que "p", "pp" et "inspect". La méthode "p" est similaire à "puts", mais elle affichera également le type de variable.

```Ruby
# Exemple de code pour utiliser la méthode p
name = "Julie"
p "Le nom est: " + name
# Sortie: "Le nom est: Julie" => String
```

La méthode "pp" est utile pour imprimer des objets complexes tels que des tableaux ou des hashs. Elle les affichera de manière plus organisée.

```Ruby
# Exemple de code pour utiliser la méthode pp
numbers = [1, 2, 3, 4]
pp numbers
# Sortie: [1, 2, 3, 4]
```

Enfin, la méthode "inspect" est similaire à "pp", mais elle vous permet de personnaliser la sortie en fournissant un bloc de code.

```Ruby
# Exemple de code pour utiliser la méthode inspect
age = 25
puts age.inspect { |num| "L'âge est #{num}" }
# Sortie: L'âge est 25
```

## Voir aussi

Pour en savoir plus sur l'impression de sortie de débogage en Ruby, consultez ces liens utiles :

- [Documentation officielle de Ruby](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Guide de débogage Ruby de The Odin Project](https://www.theodinproject.com/paths/full-stack-ruby-on-rails/courses/ruby-programming/lessons/debugging)
- [Article sur la détection d'erreurs avec print et puts en Ruby](https://www.digitalocean.com/community/tutorials/how-to-debug-ruby-processes-with-print-and-puts-statements-fr)