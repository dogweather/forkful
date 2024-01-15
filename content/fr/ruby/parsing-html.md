---
title:                "Analyse de html"
html_title:           "Ruby: Analyse de html"
simple_title:         "Analyse de html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données en ligne, il est probable que vous ayez besoin de les extraire ou de les utiliser à un moment donné. Cela peut être pour le web scraping, l'analyse de données ou tout autre projet qui nécessite l'utilisation de données HTML. L'analyse HTML est un moyen efficace de récupérer des informations précieuses à partir de pages web et peut être réalisée facilement en utilisant Ruby.

## Comment

Pour analyser du HTML en utilisant Ruby, nous pouvons utiliser une gem appelée Nokogiri. Cette gem est une bibliothèque d'analyse HTML puissante et facile à utiliser. Tout d'abord, nous devons l'installer en utilisant la commande suivante dans notre terminal :

```
gem install nokogiri
```

Ensuite, nous pouvons commencer à utiliser Nokogiri pour analyser et extraire des données HTML. Par exemple, si nous voulons extraire tous les liens d'une page web, nous pouvons utiliser le code suivant :

```
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(URI.open('https://www.example.com'))
links = doc.css('a')
links.each do |link|
  puts link['href']
end
```

Ce code utilise la méthode `css` pour sélectionner tous les éléments `<a>` de la page et la méthode `each` pour les afficher un par un. Nous pouvons également utiliser Nokogiri pour extraire d'autres éléments tels que les paragraphes, les images ou même des données spécifiques à partir d'éléments avec des attributs spécifiques.

## Plongée en profondeur

Nokogiri utilise XPath et CSS pour sélectionner des éléments dans un document HTML, ce qui le rend très flexible et puissant. Nous pouvons également utiliser des méthodes comme `at_css` pour sélectionner un seul élément plutôt qu'une liste.

De plus, Nokogiri peut également être utilisé pour analyser du XML et d'autres types de documents similaires à HTML. Il offre également des fonctionnalités avancées telles que la possibilité de nettoyer et de modifier le HTML en utilisant des méthodes telles que `remove` et `replace`. 

## À voir également

Pour en savoir plus sur l'analyse HTML en utilisant Ruby, vous pouvez consulter les ressources suivantes :
- [Getting Started with Nokogiri](https://nokogiri.org/tutorials/getting_started.html)
- [Ruby Nokogiri Tutorial](https://www.tutorialspoint.com/ruby/nokogiri_html_parsing.htm)
- [Introduction to Web Scraping with Nokogiri](https://www.sitepoint.com/web-scraping-with-nokogiri/)