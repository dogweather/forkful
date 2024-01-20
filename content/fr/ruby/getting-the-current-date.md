---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Récupérer la date actuelle signifie obtenir la date et l'heure au moment où le code est exécuté. Les programmeurs font cela pour suivre des événements en temps réel, étiqueter des données avec un horodatage, ou comparer les dates.

## Comment faire :
Voici un moyen simple de le faire. Juste utiliser `DateTime.now`.

```Ruby
require 'date'

now = DateTime.now
puts now
```

Quand vous exécutez ça, vous obtiendrez quelque chose comme ceci :

```Ruby
2022-03-27T13:42:00+02:00
```

## Plongée en Profondeur
La manière dont Ruby gère le temps et la date a une longue histoire. À l'origine, il n'y avait que la classe Time. Mais ensuite, les classes Date et DateTime ont été introduites pour donner plus de flexibilité.

Si vous voulez juste l'heure, vous pouvez utiliser la classe Time :

```Ruby
now = Time.now
puts now
```

Mais si vous voulez quelque chose comme une date julienne ou le jour de l'année, vous auriez besoin de la classe Date.

En ce qui concerne l'implémentation, la plupart du temps, les dates et les heures de Ruby sont gérées par le système d'exploitation sous-jacent. Donc, si vous exécutez votre code sur une machine avec un fuseau horaire différent, vous obtiendrez une heure différente.

## Voir aussi 
Pour plus d'informations, vous pouvez consulter ces liens :

[Ruby Doc Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html),
[Ruby Doc Time](https://ruby-doc.org/core-3.0.0/Time.html),
[Ruby Doc DateTime](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html).