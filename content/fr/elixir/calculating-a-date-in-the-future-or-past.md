---
title:    "Elixir: Calculer une date dans le futur ou le passé"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#Pourquoi
Travailler avec des dates peut être un aspect important de la programmation dans de nombreux projets. Que ce soit pour planifier des événements, pour des rappels ou pour des opérations bancaires, il est souvent nécessaire de calculer une date dans le futur ou dans le passé. Dans cet article, nous allons découvrir comment le faire efficacement en utilisant le langage de programmation Elixir.

## Comment faire
Pour calculer une date dans le futur ou dans le passé en Elixir, nous allons utiliser le module `Calendar` qui fournit des fonctions utiles pour travailler avec les dates et les heures.

Pour commencer, nous devons d'abord importer ce module en utilisant l'instruction `import Calendar` en haut de notre fichier. Ensuite, nous pouvons utiliser la fonction `add` pour ajouter un nombre spécifié de jours à une date donnée. Par exemple, si nous souhaitons calculer la date dans deux semaines à partir d'aujourd'hui, nous pouvons utiliser le code suivant :

```Elixir
import Calendar
today = Date.today()
date_dans_deux_semaines = add(today, 14)
IO.puts date_dans_deux_semaines
```
La sortie de ce code sera la date dans deux semaines à partir d'aujourd'hui au format `YYYY-MM-DD`. Nous pouvons également utiliser la fonction `subtract` pour calculer une date dans le passé en soustrayant un certain nombre de jours à une date donnée.

Mais que se passe-t-il si nous souhaitons calculer une date en tenant compte des années bissextiles ? Dans ce cas, nous pouvons utiliser la fonction `adjust` qui prend également en compte le fait que le nombre de jours dans une année peut varier. Par exemple, pour calculer la date dans un an à partir d'aujourd'hui, nous pouvons utiliser le code suivant :

```Elixir
import Calendar
today = Date.today()
date_dans_un_an = adjust(today, years: 1)
IO.puts date_dans_un_an
```

La sortie de ce code sera la date dans un an à partir d'aujourd'hui, en prenant en compte les années bissextiles.

## Plongée en profondeur
Si nous voulons aller encore plus loin dans la manipulation des dates en Elixir, nous pouvons également utiliser le module `Timex` qui étend les fonctionnalités du module `Calendar` en incluant des fonctions plus avancées telles que la prise en compte des fuseaux horaires, la conversion entre différents formats de date, et bien plus encore.

Pour utiliser `Timex`, nous devons d'abord l'ajouter dans notre fichier `mix.exs` en tant que dépendance, puis exécuter `mix deps.get` dans notre terminal pour installer le package. Ensuite, nous pouvons importer le module en utilisant `import Timex` et utiliser les fonctions telles que `shift` pour calculer une date en fonction d'un décalage spécifié, ou `parse` pour convertir une chaîne de caractères en date.

Pour plus d'informations sur le module `Timex`, vous pouvez consulter sa documentation officielle.

## Voir aussi
- [Documentation officielle du module Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Documentation officielle du module Timex](https://hexdocs.pm/timex)
- [Article sur la manipulation des dates en Elixir](https://www.dreamingechoes.com/manipulating-dates-in-elixir/) from Dreaming Echoes