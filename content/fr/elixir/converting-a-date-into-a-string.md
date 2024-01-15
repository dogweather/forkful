---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Elixir: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser à convertir une date en chaîne de caractères en utilisant Elixir. En réalité, cette opération peut être très utile dans de nombreuses situations, telles que la création de rapports ou l'affichage de dates dans un format spécifique.

## Comment faire

La première étape pour convertir une date en chaîne de caractères est de créer un objet de type ```DateTime``` en utilisant la fonction ```DateTime.from_naive/2```. Passons en revue un exemple simple :

```Elixir
date = DateTime.from_naive({{2021, 3, 15}, {15, 30, 0}}, "Etc/UTC")
```
Cette fonction prend en paramètres une liste composée de trois éléments, représentant respectivement l'année, le mois et le jour, ainsi qu'une liste contenant l'heure, les minutes et les secondes. Nous pouvons ensuite utiliser la fonction ```DateTime.to_string/2``` pour convertir notre objet en une chaîne de caractères dans le format désiré :

```Elixir
DateTime.to_string(date, "EEEE, d MMMM yyyy")
```
La sortie de cet exemple serait "lundi, 15 mars 2021". Vous pouvez également utiliser différents formats tels que "dd/mm/yyyy" pour obtenir "15/03/2021".

## Plongée en profondeur

Il est important de noter que la conversion de dates en chaînes de caractères peut être un processus complexe, surtout lorsqu'il s'agit d'obtenir un format précis ou la prise en compte de différents fuseaux horaires. Elixir offre de nombreuses fonctions utiles pour traiter les dates, telles que ```DateTime.from_iso8601/2``` pour convertir une date à partir d'une chaîne au format ISO 8601.

Il est également possible de spécifier une langue pour afficher la date dans le format correspondant. Par exemple, si vous souhaitez afficher la date en français, vous pouvez utiliser la fonction ```DateTime.to_string/3``` et spécifier "fr" en tant que deuxième paramètre :

```Elixir
DateTime.to_string(date, "EEEE, d MMMM yyyy", "fr")
```

## Voir aussi

Pour en savoir plus sur les différentes fonctions et options de formatage disponibles pour les dates en Elixir, vous pouvez consultez la documentation officielle sur les dates : [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)

Pour approfondir vos connaissances en programmation avec Elixir, voici quelques ressources utiles :

- [https://elixir-lang.org/](https://elixir-lang.org/) (Site officiel d'Elixir)
- [https://learnxinyminutes.com/docs/elixir/](https://learnxinyminutes.com/docs/elixir/) (Tutoriel rapide pour apprendre les bases d'Elixir)
- [https://github.com/phoenixframework/phoenix](https://github.com/phoenixframework/phoenix) (Framework web pour Elixir)

N'oubliez pas de pratiquer et de vous amuser avec Elixir !