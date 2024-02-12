---
title:                "Obtenir la date actuelle"
date:                  2024-02-03T19:10:30.876893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Récupérer la date actuelle est une tâche essentielle dans presque tous les projets de programmation, que ce soit pour consigner des activités dans une application ou pour générer des rapports avec des dates. En Ruby, cela peut être facilement accompli en utilisant la bibliothèque standard, simplifiant les opérations qui impliquent des dates.

## Comment faire :
La bibliothèque standard de Ruby inclut les classes `Date` et `Time` pour gérer les dates et le temps. Voici comment obtenir la date actuelle :

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Exemple de sortie : 
```
2023-04-12
```

Pour inclure l'heure avec la date, la classe `Time` de Ruby est plus appropriée :

```ruby
current_time = Time.now
puts current_time
```

Exemple de sortie : 
```
2023-04-12 14:33:07 +0200
```

Si vous avez besoin de plus de fonctionnalités, comme la gestion des fuseaux horaires, vous pourriez vouloir utiliser un gemme tiers comme `ActiveSupport` (partie de Rails mais peut être utilisé de manière autonome).

Tout d'abord, ajoutez `activesupport` à votre Gemfile et exécutez `bundle install` :

```ruby
gem 'activesupport'
```

Ensuite, utilisez-le pour gérer les fuseaux horaires :

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Définissez votre fuseau horaire souhaité
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Exemple de sortie :
```
Mer, 12 Avr 2023 08:33:07 EDT -04:00
```
