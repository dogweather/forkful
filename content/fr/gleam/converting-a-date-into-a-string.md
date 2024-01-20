---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Convertir une date en chaîne signifie transformer une représentation de date en un format de texte. Les développeurs le font pour une meilleure lisibilité des données et une facilitation de stockage ou de transfert.

## Comment faire :
```Gleam
import gleam/date.{Date, format}

let ma_date = Date.new(2022, 12, 31)
let ma_chaine = format(ma_date, "{YYYY}/{MM}/{DD}")

println(ma_chaine)
```
Sortie :
```
2022/12/31
```

## Plongée Plus Profonde
Au fil des années, les langages de programmation ont inclus diverses manières de gérer les dates. Débutant avec des formats texte simples, nous avons évolué vers des types de dates richement modélisés. Gleam est inspiré par ce riche héritage. 

D'autres langages utilisent des approches similaires. Par exemple, en Javascript, on utilise `Date.prototype.toISOString()`. En Python, c'est `strftime()`. 

Gleam utilise un format de date simple, indépendamment du fuseau horaire. Pour gérer les fuseaux horaires, vous auriez besoin d'utiliser d'autres bibliothèques ou des fonctionnalités natives du système d'exploitation.

## Voir Aussi
* Documentation officielle Gleam sur les dates : https://hexdocs.pm/gleam_stdlib/gleam/date.html
* Python strftime guide : https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
* Javascript Date.prototype.toISOString() : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString