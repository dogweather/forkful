---
aliases:
- /fr/ruby/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:05.435035-07:00
description: "Les expressions r\xE9guli\xE8res (regex) en Ruby sont des motifs utilis\xE9\
  s pour associer des combinaisons de caract\xE8res dans des cha\xEEnes de caract\xE8\
  res,\u2026"
lastmod: 2024-02-18 23:09:09.393955
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) en Ruby sont des motifs utilis\xE9\
  s pour associer des combinaisons de caract\xE8res dans des cha\xEEnes de caract\xE8\
  res,\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières (regex) en Ruby sont des motifs utilisés pour associer des combinaisons de caractères dans des chaînes de caractères, permettant aux développeurs de rechercher, d'associer et de manipuler du texte de manière efficace. Les programmeurs utilisent regex pour des tâches telles que la validation, l'analyse syntaxique, et la manipulation de chaînes de caractères, ce qui en fait un outil indispensable pour le traitement de texte.

## Comment faire :
### Association Basique
Pour comparer une chaîne de caractères à un motif simple, vous pouvez utiliser la méthode `match`. Ci-dessous, nous vérifions si le mot "Ruby" existe dans une chaîne de caractères donnée.

```ruby
if /Ruby/.match("Bonjour, Ruby !")
  puts "Correspondance trouvée !"
end
# Sortie : Correspondance trouvée !
```

### Association de Motifs avec Variables
Vous pouvez interpoler des variables dans votre regex en utilisant la syntaxe `#{}`, rendant vos motifs dynamiques.

```ruby
language = "Ruby"
if /#{language}/.match("Programmer en Ruby est amusant.")
  puts "On parle de Ruby !"
end
# Sortie : On parle de Ruby !
```

### Utilisation des Regex pour la Substitution
La méthode `gsub` vous permet de remplacer chaque occurrence d'un motif par une chaîne de remplacement spécifiée.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Sortie : barbarbar
```

### Capture
Les parenthèses dans un regex sont utilisées pour capturer des parties d'une correspondance. La méthode `match` retourne un objet `MatchData`, que vous pouvez utiliser pour accéder aux captures.

```ruby
match_data = /(\w+): (\d+)/.match("Âge : 30")
puts match_data[1] # Étiquette capturée
puts match_data[2] # Valeur capturée
# Sortie :
# Âge
# 30
```

### Utilisation de Bibliothèques Tierces
Bien que la bibliothèque standard de Ruby soit puissante, vous pourriez parfois avoir besoin de fonctionnalités plus spécialisées. Un gem populaire pour travailler avec les regex est `Oniguruma`, qui fournit des fonctionnalités regex supplémentaires au-delà du moteur regex intégré de Ruby.

Installez-le en utilisant :
```bash
gem install oniguruma
```

Un exemple d'utilisation pourrait ressembler à ceci (en supposant que vous avez requis `oniguruma` après l'avoir installé) :

```ruby
# C'est un exemple plus avancé et pourrait nécessiter une configuration supplémentaire
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("Le numéro est 42.")
puts match_data[1]
# Sortie : 42
```

Rappelez-vous, bien que puissantes, les expressions régulières peuvent devenir complexes et difficiles à gérer pour des motifs plus compliqués. Visez la lisibilité et envisagez des méthodes alternatives si votre regex devient trop alambiquée.
