---
title:    "Ruby: Génération de nombres aléatoires"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez un programme en Ruby, il est parfois utile de pouvoir générer des nombres aléatoires. Que ce soit pour créer des jeux, des simulations ou tout simplement pour ajouter une dose d'imprévisibilité à votre code, la génération de nombres aléatoires peut être un outil très pratique. Dans cet article, nous allons découvrir comment générer des nombres aléatoires en utilisant Ruby.

## Comment faire

Pour générer un seul nombre aléatoire en Ruby, vous pouvez utiliser la méthode `rand` suivie d'un nombre entier entre parenthèses représentant la limite supérieure de la plage de nombre aléatoires. Par exemple, si vous souhaitez générer un nombre aléatoire compris entre 1 et 10, vous pouvez utiliser `rand(10)`. Voici un exemple de code :

```Ruby
puts rand(10)
```

Ce code pourrait générer un nombre aléatoire comme 7. Vous pouvez également utiliser `rand` sans paramètre pour générer un nombre aléatoire entre 0 et 1. Pour générer plusieurs nombres aléatoires, vous pouvez utiliser une boucle `times` en spécifiant le nombre de fois que vous souhaitez générer un nombre aléatoire. Voici un exemple :

```ruby
5.times do
puts rand(100)
end
```

Cela générera 5 nombres aléatoires entre 0 et 100. Vous pouvez également utiliser la méthode `rand` avec des flottants pour générer des nombres aléatoires avec des décimales.

## Plongée profonde

Maintenant que vous connaissez les bases pour générer des nombres aléatoires en utilisant Ruby, vous vous demandez peut-être comment cela fonctionne réellement. En fait, les nombres aléatoires sont générés à l'aide d'un algorithme qui utilise une graine (seed en anglais) pour calculer le nombre aléatoire. La graine est un nombre qui est donné à l'algorithme pour qu'il puisse générer des nombres aléatoires. Si vous ne spécifiez pas de graine, Ruby utilisera par défaut la date et l'heure actuelles comme graine. Cela signifie que chaque fois que vous exécutez votre code, vous obtiendrez des nombres différents. Si vous voulez toujours obtenir les mêmes nombres aléatoires, vous pouvez spécifier une graine en utilisant `srand` suivi du nombre de votre choix. Voici un exemple :

```ruby
srand 42
puts rand(10)
```

Ce code générera toujours le nombre 6 car nous avons spécifié la même graine à chaque exécution.

## Voir aussi

Pour plus d'informations sur la génération de nombres aléatoires en Ruby, vous pouvez consulter ces articles (en anglais) :

- [Ruby Random Library Documentation](https://ruby-doc.org/core-2.7.1/Random.html)
- [Using Random Numbers in Ruby](https://medium.com/rubycademy/using-random-numbers-in-ruby-fe1b4ade6710)