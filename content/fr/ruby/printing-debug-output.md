---
title:                "Affichage du débogage"
html_title:           "Ruby: Affichage du débogage"
simple_title:         "Affichage du débogage"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginons que vous soyez en train de résoudre un problème dans votre programme Ruby. Vous avez peut-être remarqué un comportement étrange ou des erreurs qui se produisent. Dans de telles situations, il peut être utile d'imprimer des messages de débogage pour comprendre ce qui se passe dans votre code. Grâce à ces messages, vous pouvez suivre l'exécution de votre programme et localiser plus facilement l'origine du problème.

## Comment faire

Pour imprimer des messages de débogage dans votre code Ruby, vous pouvez utiliser la méthode `puts` ou `p` suivie du contenu que vous souhaitez afficher entre parenthèses. Par exemple:

```ruby
puts "Début du programme"

# du code

p "Variable x = #{x}"

# encore du code

puts "Fin du programme"
```
Ici, nous utilisons `puts` pour afficher le texte "Début du programme" et "Fin du programme". Nous utilisons également `p` pour afficher la valeur de la variable `x` en utilisant une interpolation de chaîne de caractères avec `#{}`. Les messages de débogage doivent être placés à des endroits stratégiques dans votre code pour vous aider à comprendre les différentes étapes de l'exécution.

## Plongée en profondeur

En plus des méthodes `puts` et `p`, il existe également la méthode `print`. La différence entre `print` et `puts` est que `print` n'ajoute pas de saut de ligne à la fin de la sortie, tandis que `puts` le fait. Vous pouvez également utiliser `inspect` pour afficher plus d'informations sur un objet, comme ceci:

```ruby
array = [1, 2, 3]
puts array
print array
puts array.inspect
```
Dans cet exemple, `puts array` affichera chaque élément du tableau sur une nouvelle ligne, `print array` imprimera le tableau entier sur une seule ligne et `puts array.inspect` affichera également le tableau entier mais avec plus d'informations sur le type d'objet et les éléments qui le composent.

## Voir aussi

- [Documentation officielle de Ruby sur les méthodes d'impression](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [Guide de débogage Ruby pour débutants](https://www.rubyguides.com/2015/03/ruby-debugging/)
- [Tutoriel vidéo sur l'impression de messages de débogage en Ruby](https://www.youtube.com/watch?v=WwQk6hsWaKY)