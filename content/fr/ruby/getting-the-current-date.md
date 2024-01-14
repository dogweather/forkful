---
title:                "Ruby: Obtenir la date actuelle"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Obtenir la date et l'heure courantes est une fonctionnalité très utile dans la programmation. Cela permet d'afficher des informations en temps réel, de créer des calendriers, des horloges, ou encore de trier des données par date. Dans cet article, nous allons vous montrer comment obtenir la date et l'heure en utilisant Ruby.

## Comment faire

Pour obtenir la date et l'heure en Ruby, nous allons utiliser la méthode `Time.now`. Elle renvoie un objet de la classe `Time` qui contient la date et l'heure actuelles. Voici un exemple de code :

```Ruby
current_time = Time.now
puts current_time
```

Lorsque vous exécutez ce code, vous obtiendrez quelque chose comme ceci :

```Shell
2021-11-30 20:15:17 +0100
```

Comme vous pouvez le voir, la date et l'heure sont affichées au format `année-mois-jour heure:minute:seconde fuseau horaire`. Cela peut varier en fonction de votre fuseau horaire. Vous pouvez également utiliser les méthodes `current_time.year`, `current_time.month`, `current_time.day`, `current_time.hour`, `current_time.min` et `current_time.sec` pour obtenir des informations spécifiques sur la date et l'heure.

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date et l'heure en utilisant `Time.now`, vous vous demandez peut-être comment cette méthode fonctionne réellement en interne. Tout d'abord, `Time.now` utilise en fait la méthode statique `Time.new` pour créer un nouvel objet `Time` avec la date et l'heure actuelles. Ensuite, il applique un formatage à cette date et heure avant de la retourner.

De plus, la classe `Time` offre différentes méthodes pour manipuler les objets de date et d'heure. Par exemple, vous pouvez utiliser `Time.parse` pour convertir une chaîne de caractères en objet `Time`, ou `Time.at` pour créer un objet `Time` à partir d'un nombre de secondes écoulées depuis le 1er janvier 1970.

## Voir aussi

- [La documentation officielle de Ruby pour Time](https://ruby-doc.org/core-3.0.2/Time.html)
- [Les différentes méthodes de la classe `Time`](https://ruby-doc.org/core-3.0.2/Time.html#method-c-new)
- [Un tutoriel sur les dates et heures en Ruby](https://www.rubyguides.com/2015/05/ruby-time/)
- [Un guide complet sur la gestion des dates et heures en Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-ruby)