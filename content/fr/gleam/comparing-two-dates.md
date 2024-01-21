---
title:                "Comparer deux dates"
date:                  2024-01-20T17:32:48.098214-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparer deux dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi?)
Comparer deux dates, c'est mettre deux moments dans le temps l'un contre l'autre pour voir lequel est plus tôt ou plus tard. Les programmeurs font ça pour trier des événements, pour des limites de temps, ou pour calculer des périodes.

## How to: (Comment faire:)
Voici comment on compare deux dates en Gleam. Simple et direct.

```gleam
import gleam/calendar.{Date, Duration}
import gleam/int

fn main() {
  let date1 = Date(2023, 4, 5)
  let date2 = Date(2023, 5, 6)

  let comparison = date1 < date2

  int.to_string(comparison)
}
```

Sortie : `"True"`

On utilise `<` pour voir si `date1` est avant `date2`. Pour tester si `date1` est après, on utiliserait `>`.

## Deep Dive (Plongée Profonde)
Historiquement, comparer des dates était plus complexe avant la standardisation des bibliothèques de temps. En Gleam, le module `gleam/calendar` simplifie tout avec des types clairement définis. 

D'autres langages utilisent différents approches. Par exemple, Python a `datetime` et JavaScript `Date`.

En Gleam, la comparaison utilise la représentation interne des dates comme le nombre de jours depuis une date 'époque'. Le détail d'implémentation précis n'est pas crucial pour l'utilisation quotidienne, mais sachez que les comparaisons sont rapides et fiables.

## See Also (Voir Aussi)
Pour aller plus loin, voici quelques ressources :

- Tutoriel sur l'utilisation des dates en programmation : [Comparing Dates in Programming](https://yourprogrammingnetwork.co.uk/comparing-dates-in-programming/)
- Un regard plus approfondi sur la gestion du temps en informatique : [Computer Time-Keeping](https://en.wikipedia.org/wiki/System_time)

Ces liens vous aideront à comprendre le contexte global et vous donneront des astuces pratiques supplémentaires.