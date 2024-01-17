---
title:                "Analyse de code HTML"
html_title:           "Ruby: Analyse de code HTML"
simple_title:         "Analyse de code HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Le parsing HTML est le processus consistant à analyser et traiter le code HTML d'une page web afin d'en extraire des données structurées. Les programmeurs le font souvent pour collecter des informations à des fins de web scraping, d'analyse de données ou de création d'outils qui automatiquement remplissent des formulaires en ligne.

## Comment faire:

Voici un exemple de code Ruby pour parser du HTML en utilisant la gem Nokogiri:

```ruby
require 'nokogiri'
require 'open-uri'

# Obtenez le HTML d'une page web
html = open("https://www.example.com").read

# Analysez le code avec Nokogiri
doc = Nokogiri::HTML(html)

# Obtenez tous les éléments avec une balise "p"
paragraphes = doc.css("p")

# Parcourez les éléments et imprimez leur contenu
paragraphes.each do |p|
  puts p.text
end
```

Résultat:

```ruby
"Bienvenue sur notre site web!"
"Ceci est un exemple de page web."
"Connectez-vous pour accéder à plus de contenu."
```

## Plongée en profondeur:

Le parsing HTML est devenu populaire dans les années 1990 avec la montée du web. Aujourd'hui, il existe plusieurs alternatives telles que le parsing basé sur des expressions régulières ou l'utilisation de bibliothèques JavaScript comme Cheerio.

Pour l'implémentation, Nokogiri est une gem populaire et puissante qui utilise la bibliothèque libxml pour traiter le HTML. Elle offre une large gamme de méthodes pour manipuler et extraire des données à partir du code HTML.

## Voir aussi:

- [Nokogiri - Documentation officielle](https://www.nokogiri.org)
- [Web scraping avec Ruby - Un guide pratique](https://rubyguides.com/2018/08/ruby-web-scraping/)