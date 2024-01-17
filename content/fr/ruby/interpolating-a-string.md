---
title:                "Interpoler une chaîne de caractères"
html_title:           "Ruby: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que l'interpolation de chaîne en Ruby et pourquoi les programmeurs l'utilisent-ils ?

L'interpolation de chaîne en Ruby est une fonction qui permet d'insérer des variables dans une chaîne de caractères. Les programmeurs l'utilisent pour simplifier le processus d'affichage de données dans leurs programmes et pour rendre les chaînes de caractères plus dynamiques.

Comment faire:

```Ruby
name = "Alice"
puts "Bonjour #{name}!" 
```

Output: Bonjour Alice!

Vous pouvez également utiliser l'interpolation de chaîne pour effectuer des opérations arithmétiques ou appeler des méthodes sur des variables. Par exemple :

```Ruby
age = 27
born_year = "Vous êtes né en #{Time.now.year - age}."
puts born_year
```

Output: Vous êtes né en 1994.

Plongée plus profonde :

L'interpolation de chaîne a été introduite en Ruby en 1993 par Yukihiro Matsumoto. Avant cela, les programmeurs utilisaient l'opérateur de concaténation (symbole +) pour insérer des variables dans des chaînes de caractères. Cependant, cela rendait le code plus difficile à lire et à maintenir. Aujourd'hui, il existe d'autres alternatives à l'interpolation de chaîne, telles que la méthode string format, mais elle est toujours largement utilisée en raison de sa simplicité et de sa facilité d'utilisation.

Voir aussi :

Si vous souhaitez en savoir plus sur l'interpolation de chaîne en Ruby, vous pouvez consulter la documentation officielle de Ruby (https://ruby-doc.org/core-3.0.1/doc/syntax/literals_rdoc.html#label-Percent+Strings) ou des tutoriels en ligne. Vous pouvez également explorer d'autres fonctionnalités de Ruby telles que les boucles, les méthodes et les classes pour améliorer vos compétences en programmation.