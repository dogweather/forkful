---
title:    "Ruby: Fusionner des chaînes de caractères"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi 

La concaténation de chaînes de caractères est une opération courante en programmation Ruby. Elle permet de combiner plusieurs chaînes ensemble pour en créer une seule nouvelle. Cela peut être utile lorsque vous travaillez avec des données ou des textes et que vous souhaitez les mettre en forme d'une certaine manière. 

## Comment faire 

Pour concaténer des chaînes de caractères en Ruby, vous pouvez utiliser l'opérateur `+` ou la méthode `concat`. Voici un exemple de code pour concaténer deux chaînes : 

```Ruby 
prenom = "Lucie" 
nom = "Dupont" 

puts prenom + " " + nom # Affiche "Lucie Dupont"
prenom.concat(" ", nom) # Concatène les deux chaînes en une seule "Lucie Dupont"
```

Dans cet exemple, nous utilisons à la fois l'opérateur `+` et la méthode `concat` pour concaténer les chaînes. Vous pouvez utiliser celui qui vous convient le mieux en fonction de votre code et de vos préférences. 

## Approfondissement 

La concaténation de chaînes de caractères peut sembler simple, mais il y a quelques points à garder à l'esprit lors de son utilisation. Tout d'abord, rappelez-vous que les chaînes de caractères sont immuables en Ruby, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Cela signifie que chaque fois que vous concaténez des chaînes, une nouvelle chaîne est créée à chaque fois. Donc, si vous avez besoin de concaténer un grand nombre de chaînes, cela peut ralentir votre code. 

De plus, il est important de noter que la concaténation peut également être effectuée avec des variables autres que des chaînes. Par exemple, vous pouvez concaténer des entiers en les convertissant en chaînes à l'aide de la méthode `to_s`. 

## Voir aussi 

- [Documentation officielle de Ruby sur la concaténation de chaînes](https://ruby-doc.org/core-2.6.3/String.html#method-i-2B)
- [Vidéo explicative sur la concaténation de chaînes en Ruby](https://www.youtube.com/watch?v=1uumGos1xkI)
- [Article de blog sur les meilleures pratiques en matière de concaténation de chaînes en Ruby](https://blog.appsignal.com/2018/09/25/ruby-magic-concatenation-vs-interpolation.html#best-practices-for-concatenation)