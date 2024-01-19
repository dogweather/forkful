---
title:                "Analyser le HTML"
html_title:           "Kotlin: Analyser le HTML"
simple_title:         "Analyser le HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Le traitement HTML, ou parsing, est le procédé utilisé pour interpréter et analyser du HTML en des composants manipulables. Les programmeurs font cela pour extraire des données, automatiser des tâches ou affiner leur interface utilisateur.

## Comment Faire:

Pour analyser du HTML en Ruby, nous avons une gem bien connue appelée `Nokogiri`. Vous pouvez l'installer avec `gem install nokogiri`:

```ruby
# installation de la gem
gem install nokogiri
```

Voici un exemple de son utilisation :

```ruby
require 'nokogiri'
require 'open-uri'

# ouverture d'une page web
doc = Nokogiri::HTML(open("https://www.example.com"))

# récupération de tous les titres h1
doc.css("h1").each do |header|
  puts header.text
end
```

Sortie d'échantillon :

```
Voici un exemple de site Web
```

## Exploration Approfondie:

Le traitement HTML est une pratique courante qui existe depuis les premiers jours du web. Avant `Nokogiri` en Ruby, il fallait écrire des expressions régulières compliquées, ce qui pouvait devenir rapidement désordonné et difficile à gérer.

En ce qui concerne les alternatives, nous avons également `Oga` et `REXML`. `Oga` est une option plus rapide mais moins populaire, tandis que `REXML` est intégré en natif dans Ruby, cependant, son utilisation est déconseillée pour de gros morceaux de HTML.

Fonctionnellement, `Nokogiri` lit et interprète du HTML en un "document" qui peut être manipulé en Ruby. Chaque balise HTML devient un "nœud" et peut être parcourue, modifiée ou extraite.

## Pour Aller Plus Loin:

- Nokogiri Documentation: https://nokogiri.org/tutorials/parsing_an_html_xml_document.html
- Oga gem: https://github.com/YorickPeterse/oga
- Ruby REXML documentation: https://ruby-doc.org/stdlib-2.6.1/libdoc/rexml/rdoc/REXML/Document.html