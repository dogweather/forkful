---
date: 2024-01-20 17:31:51.441520-07:00
description: "Comment faire : Historiquement, la manipulation de dates en programmation\
  \ a \xE9t\xE9 complexe en raison des diff\xE9rents calendriers et fuseaux horaires.\
  \ En\u2026"
lastmod: '2024-04-05T21:53:59.836092-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la manipulation de dates en programmation a \xE9t\xE9 complexe\
  \ en raison des diff\xE9rents calendriers et fuseaux horaires."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```Ruby
require 'date'

# Calculer une date 10 jours dans le futur
dans_dix_jours = Date.today + 10
puts dans_dix_jours.to_s # => "2023-04-20" (Exemple basé sur la date actuelle)

# Calculer une date 30 jours dans le passé
il_y_a_trente_jours = Date.today - 30
puts il_y_a_trente_jours.to_s # => "2023-03-21"
```

## Plongée plus profonde :
Historiquement, la manipulation de dates en programmation a été complexe en raison des différents calendriers et fuseaux horaires. En Ruby, la classe `Date` et le module `Time` simplifient beaucoup ces opérations.

Il y a d'autres manières de calculer des dates relatives. Vous pouvez utiliser `Time` si vous avez besoin de l'heure en plus de la date, et `ActiveSupport` (Rails) offre des méthodes encore plus lisibles comme `10.days.from_now`.

L'implémentation dépend du besoin en précision et du contexte (web, application, base de données). Pour certaines opérations, tenir compte des fuseaux horaires est crucial; Ruby utilise la librairie TZInfo pour les manipulations complexes de temps.

## Voir aussi :
- [Ruby's Time class](https://ruby-doc.org/core-2.7.0/Time.html)
