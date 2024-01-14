---
title:                "Ruby: Analyser du html"
simple_title:         "Analyser du html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Ruby, vous savez probablement que le HTML est un langage utilisé pour créer des pages web. Mais pourquoi voudriez-vous le « parse » (l'analyser) en utilisant Ruby ? Eh bien, il existe de nombreuses raisons, mais les plus courantes sont l'extraction de données et la manipulation de contenu.

## Comment faire

Une des méthodes courantes pour parser le HTML en utilisant Ruby est d'utiliser la gem 'nokogiri'. Voici un exemple de code qui utilise cette gem pour extraire le texte du corps d'une page web :

```Ruby
require 'nokogiri'
require 'open-uri'

url = "https://www.example.com"
page = Nokogiri::HTML(open(url))
body = page.css('body').text

puts body
```

La gem 'nokogiri' permet d'utiliser des méthodes comme `css` pour cibler des éléments spécifiques du HTML et extraire leur contenu. Dans cet exemple, nous utilisons `text` pour obtenir le texte contenu dans l'élément `body`.

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```Ruby
This is an example website.
```

Il est également possible de manipuler directement le HTML en utilisant des méthodes telles que `add_child` pour ajouter un élément, ou `set_attribute` pour définir un attribut. La gem 'nokogiri' offre de nombreuses possibilités pour travailler avec le HTML en utilisant Ruby.

## Plongée Profonde

Si vous voulez en savoir plus sur la manière dont le HTML est structuré et comment le parser efficacement, il peut être utile de plonger plus profondément dans le sujet. Vous pouvez commencer par vous familiariser avec les sélecteurs CSS, car ceux-ci sont souvent utilisés pour cibler des éléments dans le HTML.

Ensuite, vous pouvez apprendre à utiliser les expressions régulières, qui sont très utiles pour extraire des données spécifiques à partir de blocs de texte. Savoir utiliser des expressions régulières peut grandement faciliter le parsing du HTML avec Ruby.

Enfin, n'hésitez pas à consulter la documentation de la gem 'nokogiri' pour en apprendre davantage sur toutes les fonctionnalités offertes par cette bibliothèque de parsing.

## Voir aussi

- [Documentation de la gem 'nokogiri'](https://nokogiri.org/)
- [Sélecteurs CSS](https://www.w3schools.com/cssref/css_selectors.asp)
- [Expressions régulières en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)