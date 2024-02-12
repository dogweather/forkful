---
title:                "Utilisation d'un débogueur"
aliases:
- /fr/ruby/using-a-debugger.md
date:                  2024-01-26T03:51:00.690249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Utiliser un débogueur en Ruby donne aux programmeurs un super-pouvoir pour mettre en pause leur code, inspecter les variables, et parcourir leur code ligne par ligne. Les gens le font pour écraser des bugs, comprendre le flux du code, et voir exactement ce que leurs sortilèges écrits (code) font quand la magie opère — ou pas.

## Comment faire :

Ruby est livré avec un débogueur intégré appelé `byebug`. Tout d'abord, incluez `byebug` dans votre Gemfile et exécutez `bundle install`. Ensuite, placez `byebug` juste là où vous voulez que votre programme prenne une pause.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

L'exécution de ce script va arrêter l'exécution à `byebug`, et vous serez lancé dans une session interactive où vous pourrez taper des commandes comme :

```
step
next
continue
var local
```

Un exemple de sortie vous donnera une invite ressemblant à ceci :

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Plongée profonde :

Bien avant `byebug`, les Rubyistes utilisaient `debugger` et `pry`. Ce dernier, `pry`, est plus qu'un débogueur ; c'est un REPL puissant qui peut également être utilisé pour le débogage avec le point d'arrêt `binding.pry`.

Les alternatives au `byebug` de Ruby incluent `pry-byebug`, qui combine la fonctionnalité de `pry` et de `byebug`, et `ruby-debug`, qui est un ancien gem qui n'est plus activement maintenu.

Lorsque vous invoquez `byebug`, le débogueur suspend l'exécution de votre code et vous donne un aperçu de l'exécution. Vous pouvez voir et changer les variables, sauter à différents points du code, et même exécuter du code Ruby ligne par ligne. C'est un peu comme avoir des capacités de voyage dans le temps pour votre code Ruby.

## Voir aussi :

- Répertoire GitHub de Byebug : [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Documentation de Pry : [https://github.com/pry/pry](https://github.com/pry/pry)
- Un guide pour déboguer les applications Rails : [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
