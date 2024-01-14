---
title:    "Ruby: Convertir une date en chaîne de caractères"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

Pourquoi: Snippet sur la raison pour laquelle quelqu'un voudrait convertir une date en chaîne de caractères.

La conversion d'une date en une chaîne de caractères peut sembler simple, mais c'est en fait une étape importante dans de nombreux programmes Ruby. De nombreux développeurs doivent convertir des dates pour les intégrer dans des fichiers de données ou pour l'affichage dans une interface utilisateur.

## Comment Utiliser

Pour convertir une date en chaîne de caractères en utilisant Ruby, vous pouvez utiliser la méthode `strftime` (qui signifie "string format time"). La syntaxe est la suivante: 

```Ruby
date = Time.new(2020, 10, 31)
puts date.strftime("%d/%m/%Y")
```

Dans cet exemple, nous créons une date avec l'année 2020, le mois d'octobre et le jour 31 en utilisant la classe `Time` de Ruby. Ensuite, nous utilisons la méthode `strftime` pour spécifier le format de la chaîne que nous voulons obtenir. Dans ce cas, nous demandons à Ruby de nous donner la date au format "jour/mois/année". Vous pouvez utiliser différents codes de format pour personnaliser la sortie selon vos besoins. Par exemple, `%b` pour le mois abrégé en trois lettres et `%Y` pour l'année sur quatre chiffres.

La méthode `strftime` peut également être utilisée avec des variables de type `Date` ou `DateTime` en plus de `Time`.

```Ruby
require 'date'
date = Date.new(2020, 12, 25)
puts date.strftime("%A, %d %B %Y")
```

Ici, nous utilisons la classe `Date` pour créer une date pour Noël en 2020 et nous demandons à Ruby de nous donner la date au format "jour de la semaine, jour mois année". Le résultat serait "Vendredi, 25 Décembre 2020".

## Analyse Approfondie

En regardant de plus près la méthode `strftime`, vous remarquerez qu'elle accepte un argument qui spécifie le format de la chaîne de sortie. Vous pouvez combiner plusieurs codes de format pour créer une chaîne de caractères personnalisée selon vos besoins. Les codes de format doivent être placés entre des pourcentages (`%`) dans la chaîne de format.

La table ci-dessous présente quelques-uns des codes de format les plus couramment utilisés pour convertir une date en chaîne de caractères en Ruby.

| Code de format | Description |
| ------------- | ------------- |
| %d | Jour du mois (01-31) |
| %m | Mois (01-12) |
| %y | Derniers deux chiffres de l'année (00-99) |
| %Y | Année sur quatre chiffres |
| %a | Jour de la semaine abrégé en trois lettres |
| %A | Jour de la semaine complet |
| %b | Mois abrégé en trois lettres |
| %B | Mois complet |
| %H | Heure en format 24 heures (00-23) |
| %I | Heure en format 12 heures (01-12) |
| %p | AM ou PM |
| %M | Minute (00-59) |
| %S | Seconde (00-59) |
| %Z | Fuseau horaire |

Pour une liste complète des codes de format disponibles, vous pouvez consulter la documentation officielle de [Ruby strftime](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime).

## Voir Aussi

- [La documentation officielle de Ruby sur strftime](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Un guide pratique pour convertir des dates en Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Un autre tutoriel utile sur les fonctions de date et d'heure en Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-time-in-ruby-quickstart)