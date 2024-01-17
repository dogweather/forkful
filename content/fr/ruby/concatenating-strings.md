---
title:                "Concaténation de chaînes"
html_title:           "Ruby: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi? 
Concaténer des chaînes de caractères en programmation consiste à joindre différentes chaînes de caractères en une seule. Les programmeurs le font souvent pour créer du texte dynamique en combinant des variables, des nombres ou des constantes avec des chaînes de caractères. Cela peut être utile pour l'affichage à l'écran, la création de rapports ou la manipulation de données.

## Comment faire: 
Voici un exemple de code en Ruby pour concaténer des chaînes :
***
```Ruby
nom = "Jean"
age = 25
puts "Bonjour, je m'appelle " + nom + " et j'ai " + age.to_s + " ans."
```
***
Cela produira l'output suivant:
```
Bonjour, je m'appelle Jean et j'ai 25 ans.
```

## Plongée en profondeur: 
Concaténer des chaînes de caractères est une technique courante en programmation et remonte aux premiers jours de langages comme le C et le Fortran. Les alternatives pour le concaténation incluent l'utilisation de modèles de texte ou de méthodes spécifiques de langage telles que `sprintf` en Ruby. Cependant, la concaténation reste souvent la méthode la plus simple et la plus efficace pour combiner des chaînes de caractères.

## À voir également: 
Pour en savoir plus sur la concaténation de chaînes en Ruby, consultez la documentation officielle sur les chaînes [ici](https://www.ruby-lang.org/fr/documentation/). Vous pouvez également découvrir d'autres techniques utiles pour manipuler les chaînes de caractères en explorant les fonctions de base de Ruby telles que `gsub` et `reverse`. Amusez-vous à expérimenter avec la concaténation de chaînes et voyez comment cela peut améliorer votre code !