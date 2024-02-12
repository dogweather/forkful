---
title:                "Affichage des sorties de débogage"
aliases:
- /fr/ruby/printing-debug-output.md
date:                  2024-01-20T17:53:30.417519-07:00
model:                 gpt-4-1106-preview
simple_title:         "Affichage des sorties de débogage"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
L'impression du débogage, c'est afficher des infos pour comprendre ce qui se passe dans le code. On le fait parce que ça aide à repérer les bugs rapidement.

## How to:
Imaginons que vous vouliez suivre la valeur de `x` dans une boucle. Voici comment vous pourriez faire :

```ruby
(1..5).each do |x|
  puts "La valeur de x est maintenant #{x}"
end
```

Ça affichera :

```
La valeur de x est maintenant 1
La valeur de x est maintenant 2
La valeur de x est maintenant 3
La valeur de x est maintenant 4
La valeur de x est maintenant 5
```

## Deep Dive
Historiquement, `puts` et `print` sont les commandes de base en Ruby pour l'affichage du débogage. Utiliser `p` est une alternative qui donne plus d'infos, car elle appelle `.inspect` sur l'objet. Dans le contexte de tests, on use de bibliothèques comme `byebug` ou `pry` pour un contrôle plus poussé.

Au niveau de l'implémentation, `puts` ajoute une nouvelle ligne après l'affichage, tandis que `print` non :

```ruby
print "Bonjour"
print " le monde"
# Affiche: Bonjour le monde

puts "Bonjour"
puts "le monde"
# Affiche:
# Bonjour
# le monde
```

L'utilisation de ces méthodes est simple, mais en abuser peut rendre le code plus difficile à lire et maintenir.

## See Also
Pour approfondir, voici quelques ressources :

- Documentation officielle de Ruby pour `puts`, `print`, et `p` : [ruby-doc.org](https://ruby-doc.org/core-3.1.0/IO.html#method-i-puts)
- Byebug, un débuggeur pour Ruby : [github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry, une alternative puissante pour explorer et déboguer votre code : [pry.github.io](https://pry.github.io)
