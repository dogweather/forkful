---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.870387-07:00
description: "Analyser du HTML signifie d\xE9composer un bloc de code HTML pour en\
  \ saisir la structure et le contenu. Les programmeurs le font pour extraire des\
  \ donn\xE9es,\u2026"
lastmod: '2024-03-13T22:44:58.417566-06:00'
model: gpt-4-0125-preview
summary: "Analyser du HTML signifie d\xE9composer un bloc de code HTML pour en saisir\
  \ la structure et le contenu. Les programmeurs le font pour extraire des donn\xE9\
  es,\u2026"
title: Analyse Syntaxique du HTML
weight: 43
---

## Quoi & Pourquoi ?
Analyser du HTML signifie décomposer un bloc de code HTML pour en saisir la structure et le contenu. Les programmeurs le font pour extraire des données, manipuler le contenu, ou migrer des informations entre formats et systèmes.

## Comment faire :
Pour analyser du HTML en Ruby, installez le gem 'Nokogiri' avec `gem install nokogiri`. Nokogiri est comme un couteau suisse pour travailler avec HTML et XML en Ruby. Voici un rapide exemple :

```ruby
require 'nokogiri'
require 'open-uri'

# Charger le contenu HTML d'un site web
html_content = URI.open('http://example.com').read

# Analyser le HTML
doc = Nokogiri::HTML(html_content)

# Extraire le titre
title = doc.xpath('//title').text
puts "Le titre de la page est : #{title}"
```

Cela affiche quelque chose comme : `Le titre de la page est : Domaine Exemple`.

## Plongée Profonde
Au début des temps de Ruby, les options pour analyser le HTML étaient limitées. REXML était intégré mais lent. Puis Hpricot est apparu, mais il s'est éteint. Nokogiri a fait ses débuts en 2008, alliant la facilité d'Hpricot avec la vitesse et la puissance de libxml, une boîte à outils XML éprouvée.

Dans le monde de l'analyse, il y a toujours des alternatives. Certains jurent uniquement par la bibliothèque intégrée 'rexml' ou 'oga', un autre analyseur XML/HTML pour Ruby. Mais Nokogiri reste un favori pour sa robustesse et sa vitesse, sans parler de sa vaste gamme de fonctionnalités.

Sous le capot, Nokogiri convertit le HTML en un Modèle Objet de Document (DOM) – une structure arborescente. Cela facilite la navigation et la manipulation des éléments. En utilisant XPath et les sélecteurs CSS, vous pouvez localiser n'importe quelle information dont vous avez besoin.

## Voir Aussi
- Gem Nokogiri : [https://nokogiri.org/](https://nokogiri.org/)
- Documentation de rexml de Ruby : [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Analyseur alternatif 'oga' : [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Apprendre à propos de XPath : [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
