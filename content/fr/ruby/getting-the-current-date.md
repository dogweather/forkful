---
title:    "Ruby: Obtenir la date actuelle"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir obtenir la date actuelle dans votre code Ruby. Cela peut être utile pour enregistrer la date d'une transaction ou pour afficher la date sur une page Web dynamique.

## Comment faire

Il existe plusieurs façons de récupérer la date actuelle en Ruby. L'une des façons les plus simples est d'utiliser la méthode `Time.now`, qui renvoie un objet Time représentant la date et l'heure actuelles.

```Ruby
puts "La date actuelle est: #{Time.now}"
```
Output: "La date actuelle est: 2020-10-22 14:30:00 +0100"

Vous pouvez également utiliser la méthode `Date.today`, qui renvoie un objet Date représentant la date actuelle sans l'heure.

```Ruby
puts "La date actuelle est: #{Date.today}"
```
Output: "La date actuelle est: 2020-10-22"

Vous pouvez également personnaliser le format de la date en utilisant la méthode `strftime`, qui vous permet de spécifier le format de la date que vous souhaitez obtenir. Voici un exemple de code qui renvoie la date au format "jour/mois/année":

```Ruby
aujourdhui = Time.now
puts "La date actuelle est: #{aujourdhui.strftime("%d/%m/%Y")}"
```
Output: "La date actuelle est: 22/10/2020"

## Plongée en profondeur

En creusant un peu plus dans les méthodes de récupération de la date en Ruby, vous découvrirez qu'il existe de nombreuses autres méthodes pour obtenir des informations spécifiques telles que le mois, l'année, le jour de la semaine, etc. Vous pouvez également trouver des méthodes pour comparer des dates, les convertir en différents fuseaux horaires et bien plus encore.

De plus, si vous souhaitez utiliser la date dans une application Web, Ruby on Rails propose également des fonctionnalités intégrées pour manipuler facilement la date et l'heure.

## Voir aussi

- [Documentation officielle sur Time et Date en Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/time/rdoc/Time.html)
- [Utilisation des dates et heures en Ruby on Rails](https://guides.rubyonrails.org/active_support_core_extensions.html#extensions-to-date)

Merci d'avoir lu cet article sur la façon de récupérer la date actuelle en Ruby. Nous espérons que vous l'avez trouvé utile ! N'hésitez pas à explorer davantage les nombreuses fonctionnalités de Date et Time en Ruby pour améliorer vos compétences en programmation.