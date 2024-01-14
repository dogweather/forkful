---
title:                "Ruby: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Ruby, alors vous savez probablement qu'il est courant de travailler avec des dates dans votre code. Cependant, parfois, vous pourriez avoir besoin de convertir une date en une chaîne de caractères. Dans cet article, nous allons explorer pourquoi et comment effectuer cette conversion en utilisant Ruby.

## Comment faire

Tout d'abord, nous allons définir une variable contenant une date :

```
date = Time.new(2021, 10, 31)
```

Ensuite, nous allons utiliser la méthode `strftime` pour convertir cette date en une chaîne de caractères, en spécifiant un format de notre choix :

```
date.strftime("%d/%m/%Y")
```

Ce qui donnera comme résultat `"31/10/2021"`. Vous pouvez également utiliser différentes directives pour obtenir un formatage spécifique, telles que `%B` pour le mois complet en lettres ou `%H` pour l'heure au format 24h.

## Plongée en profondeur

La méthode `strftime` utilise les bibliothèques `strftime` du système pour effectuer cette conversion. Ces bibliothèques sont également utilisées par les commandes UNIX `date` et `strftime`, ce qui nous permet de bénéficier d'une grande flexibilité quant au formatage des dates en utilisant Ruby.

De plus, la méthode `strftime` peut également prendre en compte la langue et la région de votre système pour afficher les dates dans un format spécifique. Cela peut être utile si vous travaillez avec des données multilingues dans votre application.

## Voir aussi

- [La documentation officielle de Ruby sur la méthode `strftime`](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)
- [Un tutoriel en français pour formater les dates en Ruby](https://zestedesavoir.com/tutoriels/849/formater-des-dates-avec-ruby/)
- [Une autre méthode pour convertir les dates en chaînes de caractères en utilisant la classe `DateTime`](https://www.youtube.com/watch?v=fLvCCw8JUWo)