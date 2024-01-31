---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:16:29.945177-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Récupérer la date actuelle en Ruby, c'est juste savoir quel jour on est. Les développeurs le font pour enregistrer des timestamps, des fonctionnalités basées sur la date, ou simplement afficher la date d'aujourd'hui.

## How to: (Comment faire :)
Pour mettre la main sur la date d'aujourd'hui dans Ruby, rien de plus simple. Utilise la classe `Date` ou `Time` :

```ruby
require 'date'

# Utilise Date pour la date actuelle
puts Date.today
# => 2023-04-12

# Utilise Time pour la date et l'heure actuelle
puts Time.now.strftime("%Y-%m-%d")
# => 2023-04-12
```
La sortie varie selon le jour où tu exécutes le code, bien sûr.

## Deep Dive (Plongée en Profondeur)
Historiquement, Ruby n'a pas toujours eu une gestion du temps aussi robuste. Avant la standardisation, des gems comme `time` et `date` ont été créées pour enrichir les fonctionnalités de base. Il y a aussi `DateTime`, mais on l'utilise moins car `Time` gère maintenant les fuseaux horaires à merveille.

Pour les alternatifs, t'as le choix entre :
- `Chronic` pour parser des dates en texte naturel.
- `ActiveSupport::TimeWithZone` si t'es sur Rails et que tu veux gérer le temps avec des fuseaux horaires.

Concernant l'implémentation, `Time.now` te donne un objet `Time` plein d'informations, tandis que `Date.today` te file une instance de `Date` qui est plus légère, sans l'heure.

## See Also (Voir Aussi)
- [Documentation Ruby pour Time](https://ruby-doc.org/core-2.7.0/Time.html)
- [Documentation Ruby pour Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [RubyGem pour Chronic](https://rubygems.org/gems/chronic)
- [Rails API pour ActiveSupport::TimeWithZone](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
