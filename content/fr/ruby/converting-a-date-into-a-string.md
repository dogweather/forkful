---
title:                "Ruby: Convertir une date en chaîne de caractères"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire, en programmation, de convertir une date en une chaîne de caractères afin de l'afficher de manière lisible pour les utilisateurs. Cela permet également de manipuler plus facilement les dates dans le code.

## Comment faire

Pour convertir une date en une chaîne de caractères en utilisant le langage de programmation Ruby, nous pouvons utiliser la méthode `strftime` qui permet de formater une date selon un modèle spécifique. Voici un exemple de code :

```Ruby
date = Time.new(2021, 8, 15)
puts date.strftime("%d/%m/%Y")
```

Dans cet exemple, nous créons une variable `date` contenant une date précise (15 août 2021) et nous utilisons la méthode `strftime` pour la formater selon le modèle `"%d/%m/%Y"` qui correspond à "jour/mois/année". La sortie de ce code sera donc `"15/08/2021"`.

## Plongée en profondeur

La méthode `strftime` prend en paramètre un modèle de formatage qui définit la manière dont la date doit être affichée en tant que chaîne de caractères. Voici quelques modèles couramment utilisés en Ruby :

- `%D` : date au format "mm/dd/yy"
- `%F` : date au format "yyyy-mm-dd"
- `%H:%M:%S` : heure au format "hh:mm:ss"
- `%A, %d %B %Y` : date complète au format "jour, jour mois année"
- `%d/%m/%Y %H:%M` : date et heure au format "jj/mm/aa hh:mm"

Il est également possible d'utiliser les méthodes `day`, `month` et `year` pour récupérer respectivement le jour, le mois et l'année d'une date en tant qu'entiers, ce qui peut être utile pour des manipulations spécifiques avant de les convertir en chaîne de caractères.

## Voir aussi

- [Documentation officielle de Ruby sur la méthode `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Tutoriel Ruby sur la manipulation de dates et heures](https://www.rubyguides.com/2020/02/ruby-time-class/)
- [Guide complet du langage Ruby](https://ruby-doc.org/core-3.0.0/)