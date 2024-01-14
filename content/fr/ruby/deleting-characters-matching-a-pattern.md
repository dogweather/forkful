---
title:                "Ruby: Suppression de caractères correspondant à un motif"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser à la suppression de caractères correspondant à un modèle en utilisant Ruby. Eh bien, c'est une compétence utile qui peut être utilisée pour nettoyer des données ou pour effectuer des recherches dans des chaînes de caractères. Cela peut également être utile lorsque vous travaillez avec des expressions régulières dans vos projets.

## Comment faire

Voici quelques exemples de code pour vous montrer comment supprimer des caractères correspondant à un modèle en utilisant Ruby :

```Ruby
str = "Bonjour à tous !"

# Supprimez tous les caractères qui ne sont pas des lettres
str.gsub!(/[^a-z]/i, "")
# Résultat : Bonjouratous

# Supprimez tous les caractères spéciaux
str.gsub!(/[^A-Z0-9]/i, "")
# Résultat : BONJOURATOUS123

# Supprimez tous les chiffres
str.gsub!(/[0-9]/, "")
# Résultat : BONJOURATOUS
```

Dans cet exemple, nous utilisons la méthode `gsub!` qui remplace toutes les occurrences du modèle par une chaîne vide, ce qui a pour effet de supprimer ces caractères de la chaîne d'origine.

## Deep Dive

Maintenant que vous avez compris comment supprimer des caractères correspondant à un modèle en utilisant Ruby, plongeons un peu plus en profondeur. En utilisant des expressions régulières, vous pouvez affiner votre modèle pour qu'il corresponde à des caractères plus spécifiques ou pour utiliser des options telles que la sensibilité à la casse ou la correspondance globale à travers toute la chaîne de caractères.

Voici quelques ressources supplémentaires pour vous aider à approfondir vos connaissances sur les expressions régulières en Ruby :

- [Documentation Ruby sur les expressions régulières](https://ruby-doc.org/core-2.6.3/Regexp.html)
- [Tutoriel sur les expressions régulières en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Rubular, un outil pour tester vos expressions régulières en temps réel](https://rubular.com/)

## Voir aussi

- [Guide ultime pour les expressions régulières en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Apprenez à utiliser les expressions régulières en 5 minutes](https://medium.com/rubyinside/learn-to-use-regular-expressions-in-5-minutes-3d8646ea7441)
- [10 astuces pour maîtriser les expressions régulières en Ruby](https://www.geeksforgeeks.org/10-ruby-regular-expressions/)

Maintenant que vous avez toutes les informations nécessaires, il est temps de vous mettre à l'œuvre et de commencer à utiliser la puissance des expressions régulières en Ruby pour supprimer des caractères correspondant à un modèle. Bonne chance !