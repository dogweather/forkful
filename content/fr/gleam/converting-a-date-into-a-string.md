---
title:                "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une date en chaîne de caractères est souvent nécessaire dans la programmation, en particulier pour afficher des informations de date à l'utilisateur final. Cela peut également être utile pour trier et filtrer des données basées sur des dates.

## Comment faire

Pour convertir une date en chaîne de caractères en utilisant Gleam, il suffit d'utiliser la fonction `to_string` de la bibliothèque standard `gleam/time`.

```
Gleam
import gleam/time

let my_date = time.date(2021, 3, 8)
gleam/time.to_string(my_date)
```

Lorsque vous exécutez ce code, le résultat devrait être `"2021-03-08"`, le format par défaut pour la conversion de date en chaîne de caractères en Gleam.

## Plongée en profondeur

En Gleam, les dates sont représentées en utilisant la structure de données `Date` de la bibliothèque standard `gleam/time`. Cette structure contient des fonctions utiles pour travailler avec des dates, telles que `add_days` pour ajouter des jours à une date existante, `subtract_days` pour soustraire des jours, et ainsi de suite. Vous pouvez également utiliser la fonction `parse` pour convertir une chaîne de caractères en date.

```
Gleam
import gleam/time

let my_date = time.parse("2021-03-08")
time.add_days(my_date, 5)
```

Dans cet exemple, nous avons d'abord utilisé `parse` pour convertir la chaîne de caractères `"2021-03-08"` en date, puis `add_days` pour ajouter 5 jours à cette date. Le résultat sera alors une nouvelle date représentant le 13 mars 2021.

## Voir aussi

Pour en savoir plus sur les dates et les chaînes de caractères en Gleam, vous pouvez consulter ces ressources :

- [Documentation officielle de Gleam sur les dates](https://gleam.run/core/time.html)
- [Guide pratique de Gleam pour les débutants](https://gleam.run/book/tour.html)
- [Forum de la communauté Gleam pour poser des questions et partager des astuces](https://forum.gleam.run/)

N'hésitez pas à explorer et à expérimenter avec Gleam pour en apprendre davantage sur ce langage fonctionnel en plein essor. Bonne programmation !