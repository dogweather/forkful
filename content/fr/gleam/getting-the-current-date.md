---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce Que C'est et Pourquoi?

Obtenir la date actuelle est lorsque vous commandez à votre application d'afficher ou d'utiliser la date et l'heure réelles. Les programmeurs le font pour enregistrer des moments précis dans les logs d'événements, pour le dépannage, ou pour des fonctionnalités basées sur le temps dans les applications.

## Comment Faire:

Dans Gleam, nous avons besoin d'interopérer avec Erlang - la plateforme sous-jacente à Gleam.

```Gleam
import erlang/date as Date

fn get_today() {
  let date = Date.today()
  date.to_string()
}
```
La sortie sera:

```Gleam
"2022-05-25"
```

## Plus de Détails:

Historiquement, Erlang a été développé pour les systèmes de télécommunication; où la date et l'heure sont essentiels aux journaux. Ainsi, Gleam bénéficie de ces outils intégrés. 

Une alternative pourrait être de passer par le module 'calendar' pour des opérations plus complexes, comme obtenir le jour de la semaine.

Au niveau de l'implémentation, quand nous appelons `Date.today()`, nous interrogeons le système d'exploitation pour obtenir la date actuelle, que nous convertissons ensuite en format lisible à l'aide de `to_string()`.

## Voir Aussi:

- Documentation Erlang pour date module: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
- Guide d'interopérabilité Gleam-Erlang: [https://gleam.run/book/tour/erlang-interop.html](https://gleam.run/book/tour/erlang-interop.html)