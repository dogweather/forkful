---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
*Quoi & Pourquoi ?*

Les expressions régulières permettent de chercher, modifier et valider des chaînes de caractères. Les devs les utilisent pour la puissance et la flexibilité qu'elles offrent lors du traitement de texte.

## How to:
*Comment faire :*

```Ruby
# Recherche simple
phrase = "Bienvenue dans le monde de Ruby!"
if phrase =~ /Ruby/
  puts "Mot 'Ruby' trouvé!"
end
# Output: Mot 'Ruby' trouvé!

# Remplacer un mot
nouvelle_phrase = phrase.gsub(/Ruby/, 'Python')
puts nouvelle_phrase
# Output : Bienvenue dans le monde de Python!

# Valider un format d'email
email = "contact@exemple.fr"
puts "Format valide!" if email.match?(/\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i)
# Output: Format valide!
```

## Deep Dive
*Plongée en profondeur*

Les regex ont été popularisées par Perl, influençant de nombreux autres langages, y compris Ruby. Sans elles, on devrait écrire des parseurs personnalisés - fastidieux et plus sujets aux erreurs. Ruby utilise une librairie regex nommée Onigmo, une évolution d'Oniguruma. Elle supporte diverses encodages et des constructions de syntaxe avancée.

## See Also
*Voir aussi*

- Documentation Ruby sur les regex: [Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Un tutoriel interactif pour apprendre les regex: [Rubular](http://rubular.com/)