---
title:    "Ruby: Utilisation des expressions régulières"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour manipuler et rechercher des chaînes de caractères dans un programme Ruby. Elles permettent d'effectuer des opérations complexes en utilisant des motifs ou des règles. En utilisant des expressions régulières, vous pouvez décomposer une chaîne de caractères et extraire des informations spécifiques, ou même vérifier la validité d'une chaîne selon un certain format. En bref, les expressions régulières sont un moyen efficace de traiter et de manipuler les données dans votre code Ruby.

## Comment faire

Pour utiliser des expressions régulières dans votre code Ruby, vous devez d'abord les déclarer en utilisant la syntaxe `/pattern/`. Les expressions régulières consistent en des caractères spéciaux qui représentent des motifs et des règles à suivre. Par exemple, le caractère `.` représente n'importe quel caractère, tandis que `*` signifie « zéro ou plusieurs occurrences ». Voici un exemple de recherche d'une adresse e-mail valide en utilisant une expression régulière :

```Ruby
email = "example@test.com"
if email =~ /\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i
  puts "Adresse e-mail valide"
else
  puts "Adresse e-mail invalide"
end
# Output: Adresse e-mail valide
```

Vous pouvez également utiliser des expressions régulières pour remplacer ou modifier certaines parties d'une chaîne de caractères. Par exemple, si vous souhaitez remplacer tous les espaces par des tirets dans un texte, vous pouvez utiliser la méthode `sub` avec une expression régulière :

```Ruby
texte = "Ceci est un exemple de texte avec des espaces"
texte = texte.sub(/\s+/, '-')
puts texte
# Output: Ceci-est-un-exemple-de-texte-avec-des-espaces
```

## Approfondissement

Les expressions régulières peuvent sembler complexes au premier abord, mais elles peuvent être très utiles une fois que vous les maîtrisez. En plus des caractères spéciaux mentionnés précédemment, il existe également des groupes de captures, des opérateurs de quantité, des groupes de négation et bien d'autres fonctionnalités. En en apprenant davantage sur les expressions régulières, vous pourrez résoudre des problèmes plus complexes et augmenter l'efficacité de votre code.

## Voir aussi

- [Documentation sur les expressions régulières en Ruby](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Ruby regular expressions: cheat sheet](https://www.rubyguides.com/2015/06/ruby-regex/#:~:text=Ruby%20Regular%20Expressions%20Tutorial%20%E2%80%94%20A%2023%2DPage%20Cheat%20Sheet&text=Cheat%20Sheet%E2%80%9D%20of%20just%2023%20pages,-%E2%80%94Scroll%20down) (en anglais)
- [RegEx101 : pour tester et expérimenter avec des expressions régulières](https://regex101.com/) (en anglais)