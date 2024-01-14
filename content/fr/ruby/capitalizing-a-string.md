---
title:    "Ruby: Mettre en majuscule une chaîne de caractères"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de manipuler des chaînes de caractères. Et parfois, il est nécessaire de capitaliser une chaîne pour respecter certaines conventions ou pour des raisons esthétiques. Dans cet article, nous allons explorer comment capitaliser une chaîne en Ruby et approfondir les différentes méthodes disponibles.

## Comment faire

Il existe plusieurs façons de capitaliser une chaîne en Ruby, en fonction de vos besoins spécifiques. Voici quelques exemples de code avec leur sortie respective :

```ruby
# Utiliser la méthode capitalize pour capitaliser la première lettre uniquement
"bonjour tout le monde".capitalize # "Bonjour tout le monde"

# Utiliser la méthode upcase pour tout mettre en majuscule
"Bonjour tout le monde".upcase # "BONJOUR TOUT LE MONDE"

# Utiliser la méthode gsub pour remplacer la première lettre par sa version en majuscule
"bienvenue sur ruby".gsub(/^./) { |letter| letter.upcase } # "Bienvenue sur ruby"
```

Vous pouvez également utiliser la méthode `gsub` avec une expression régulière pour capitaliser toutes les lettres d'une chaîne :

```ruby
"bonjour tout le monde".gsub(/(\w+)/) { |word| word.capitalize } # "Bonjour Tout Le Monde"
```

Et si vous avez des chaînes avec des caractères accentués, vous pouvez utiliser la méthode `unicode_normalize` pour les manipuler correctement :

```ruby
"écrire en français".unicode_normalize(:nfd).capitalize # "Écrire en français"
```

## Plongée en profondeur

Maintenant que nous avons vu les différentes façons de capitaliser une chaîne, il est important de noter que ces méthodes ne modifient pas la chaîne originale, mais renvoient plutôt une nouvelle chaîne capitalisée. De plus, il existe des méthodes similaires à `capitalize` et `upcase` telles que `capitalize!` et `upcase!` qui modifient la chaîne originale.

De plus, si vous avez besoin de capitaliser les lettres après des caractères spécifiques (comme des espaces ou des tirets), vous pouvez utiliser la méthode `split` pour diviser la chaîne en parties séparées par ces caractères, puis utiliser `capitalize` sur chaque partie. Vous pouvez ensuite utiliser `join` pour recombiner ces parties en une chaîne unique.

## Voir aussi

Voici quelques ressources supplémentaires pour en savoir plus sur la manipulation de chaînes en Ruby :

- Documentation officielle sur les chaînes en Ruby : https://ruby-doc.org/core-3.0.1/String.html
- Tutoriel YouTube sur les bases de la manipulation de chaînes en Ruby : https://www.youtube.com/watch?v=uLsArArnmOU

En utilisant ces méthodes et en comprenant les concepts derrière la manipulation de chaînes en Ruby, vous serez en mesure de capitaliser facilement des chaînes et de les adapter à vos besoins spécifiques.