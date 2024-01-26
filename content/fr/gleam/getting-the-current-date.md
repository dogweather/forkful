---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:14:19.801835-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle c'est simplement demander à l'ordinateur "quelle est la date aujourd'hui?" Les programmeurs font ça pour tout : horodater les logs, mesurer le temps qui passe ou programmer des évènements futurs.

## Comment faire :
```gleam
import gleam/erlang
import gleam/calendar.{Datetime, Zone}

pub fn main() {
  // Obtenir le DateTime actuel en UTC
  let maintenant_utc = erlang.now()
  // Convertir en DateTime structuré
  let datetime_utc = Datetime.from_erlang(maintenant_utc)
  // Afficher le DateTime UTC
  erlang.display(datetime_utc)

  // Si vous voulez la date et l'heure dans un fuseau horaire spécifique
  let paris_zone = Zone.named("Europe/Paris").unwrap()
  let maintenant_paris = Datetime.from_erlang_with_zone(maintenant_utc, paris_zone)
  // Afficher le DateTime de Paris
  erlang.display(maintenant_paris)
}
```
Sortie probable :
```
Datetime(year: 2023, month: 4, day: 7, hour: 13, minute: 46, second: 34, millisecond: 657, zone: Zone.utc)
Datetime(year: 2023, month: 4, day: 7, hour: 15, minute: 46, second: 34, millisecond: 657, zone: Zone.named("Europe/Paris"))
```

## Plongée profonde
Historiquement, obtenir la date actuelle est crucial pour l'organisation et la coordination. En informatique, on prend souvent le temps universel coordonné (UTC) comme référence. Gleam s'intègre à l'écosystème Erlang/OTP, donc on utilise souvent des fonctions natives d'Erlang pour ce genre de tâche.

Concernant les alternatives, on peut citer NTP (Network Time Protocol) pour synchroniser les horloges. En Gleam, travailler avec les dates implique de manipuler des tuples Erlang ou d'utiliser des fonctions de haut niveau pour plus de commodité. 

Dans l'implémentation, la clé est de gérer correctement les différentes zones horaires. Elles peuvent affecter les applications de manière significative, particulièrement pour des utilisateurs dans différents fuseaux horaires ou pour des tâches planifiées en fonction de l'heure locale.

## Voir aussi
- [Erlang's time functions](http://erlang.org/doc/man/erlang.html#time-0)
- [About the Network Time Protocol (NTP)](https://www.ntp.org/)
