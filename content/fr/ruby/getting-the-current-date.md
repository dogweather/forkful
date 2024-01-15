---
title:                "Obtenir la date actuelle"
html_title:           "Ruby: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un s'engagerait à obtenir la date actuelle en Ruby. Eh bien, cela peut être utile dans de nombreuses situations, comme l'ajout d'un horodatage à vos données ou à vos journaux, la planification d'événements futurs, ou simplement pour afficher la date dans un format personnalisé.

## Comment faire

La bonne nouvelle est qu'il est très facile d'obtenir la date actuelle en utilisant Ruby. Voici un exemple de code qui utilise la méthode `Time.now` pour obtenir la date et l'heure actuelles :

```Ruby
current_date = Time.now
puts current_date
```

Et voici l'output que vous obtiendrez lorsque vous exécutez ce code :

```
2021-07-21 11:23:47 +0200
```

Comme vous pouvez le voir, le format par défaut pour `Time.now` est `année-mois-jour heure:minute:seconde +fuseau horaire`. Vous pouvez également formater la date selon vos préférences en utilisant la méthode `strftime` et en y passant un argument de format. Voici un exemple :

```Ruby
current_date = Time.now
puts current_date.strftime("%d/%m/%Y")
```

Et voici l'output correspondant :

```
21/07/2021
```

## Plongée en profondeur

La méthode `Time.now` est en fait une instance de la classe `Time`. Cette classe est utilisée pour représenter une date et une heure. Elle possède de nombreuses méthodes utiles pour manipuler et formater les dates. Par exemple, vous pouvez utiliser la méthode `sunday?` pour vérifier si la date actuelle est un dimanche, ou la méthode `month` pour obtenir le mois actuel.

Il convient également de mentionner que la méthode `Time.now` renvoie la date et l'heure actuelles selon le fuseau horaire de votre système. Si vous souhaitez obtenir la date et l'heure dans un fuseau horaire spécifique, vous pouvez utiliser `Time.now.utc` pour obtenir le temps universel coordonné (UTC) ou utiliser la gemme `tzinfo` pour travailler avec des fuseaux horaires spécifiques.

## Voir aussi

- [Documentation Ruby sur la classe Time](https://ruby-doc.org/core-3.0.1/Time.html)
- [Documentation Ruby sur la méthode Time.now](https://ruby-doc.org/core-3.0.1/Time.html#method-c-now)
- [Documentation Ruby sur la méthode strftime](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)