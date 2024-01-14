---
title:                "Ruby: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
 La comparaison de deux dates est une pratique courante en programmation Ruby, surtout lorsqu'il s'agit de données temporelles. Comprendre comment comparer correctement deux dates peut vous aider à mieux gérer vos données et à prendre des décisions basées sur le temps.

## Comment Faire
Pour comparer deux dates en Ruby, vous pouvez utiliser la méthode `compare` de la classe `Time`. Par exemple, si vous avez deux variables `date1` et `date2` contenant des objets de type `Time`, vous pouvez les comparer comme ceci :

```Ruby
# Exemple de comparaison de deux dates en Ruby

date1 = Time.new(2020, 1, 1)
date2 = Time.new(2020, 2, 1)

puts date1.compare(date2) # Output: -1
```

Dans cet exemple, la méthode `compare` renvoie -1 car `date2` est après `date1` dans le temps. Voici les valeurs de retour possibles pour la méthode `compare` :

- 0 : si les deux dates sont égales
- 1 : si la première date est après la deuxième date
- -1 : si la première date est avant la deuxième date

Vous pouvez également utiliser d'autres méthodes pour comparer des dates, comme `==` pour vérifier si les dates sont égales, `<` et `>` pour vérifier si une date est avant ou après une autre, et `between?` pour vérifier si une date se situe entre deux autres dates. En expérimentant avec ces méthodes, vous pourrez mieux comprendre comment comparer des dates en Ruby.

## Plongée en Profondeur
Il y a plusieurs éléments à prendre en compte lors de la comparaison de dates en Ruby. Tout d'abord, il est important de comprendre que les dates en Ruby sont représentées sous forme d'objets de type `Time`, qui contiennent des informations telles que l'année, le mois, le jour, l'heure, etc. Cela signifie que lorsque vous comparez des dates, vous comparez en fait des objets et non des chaînes de caractères.

Il est également important de noter que la méthode `compare` est la méthode par défaut pour comparer des dates en Ruby. Cependant, si vous préférez une syntaxe plus lisible, vous pouvez également utiliser la méthode `between?` en spécifiant la première et la dernière date en tant que paramètres :

```Ruby
date1 = Time.new(2020, 1, 1)
date2 = Time.new(2020, 2, 1)

if date1.between?(date2, Time.new(2020, 5, 1))
  puts "La date 1 se situe entre la date 2 et le 1er mai 2020."
end
```

Enfin, il est important de prendre en compte les différents formats de dates dans vos comparaisons. Par exemple, si vous comparez deux dates avec des fuseaux horaires différents, cela peut conduire à des résultats inattendus. Il est donc essentiel de comprendre les formats de date et d'assurer la cohérence lors de la comparaison.

## Voir Aussi
Pour plus d'informations sur la comparaison de dates en Ruby, vous pouvez consulter les ressources suivantes (en anglais) :

- La documentation officielle de Ruby sur les dates et les heures : https://ruby-doc.org/core-2.7.1/Time.html
- Un tutoriel sur la manipulation de dates en Ruby : https://www.rubyguides.com/2015/08/ruby-time/
- Un article sur les formats de dates en Ruby : https://www.sitepoint.com/ruby-date-and-time/

Merci d'avoir lu cet article sur la comparaison de dates en Ruby ! Nous espérons que cela vous aidera dans vos futurs projets de programmation.