---
date: 2024-01-26 04:35:11.070065-07:00
description: 'Comment faire : Utilisons REXML, inclus avec Ruby, pour analyser un
  extrait XML .'
lastmod: '2024-04-05T21:53:59.846952-06:00'
model: gpt-4-0125-preview
summary: Utilisons REXML, inclus avec Ruby, pour analyser un extrait XML .
title: Travailler avec XML
weight: 40
---

## Comment faire :
Utilisons REXML, inclus avec Ruby, pour analyser un extrait XML :
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Nom : #{element.attributes['name']}, Couleur : #{element.attributes['color']}"
end
```
Sortie :
```
Nom : apple, Couleur : green
Nom : banana, Couleur : yellow
```

Générer du XML est également simple :
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Sortie XML :
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Plongée profonde :
Les racines de l'XML remontent aux années 1990 en tant que sous-ensemble simplifié du SGML pour les documents web. Il est verbeux mais très structuré, et c'est pourquoi il a perduré. Ce n'est pas le seul en jeu - JSON et YAML sont devenus populaires pour leur simplicité - mais XML reste fort dans de nombreux systèmes d'entreprise et hérités.

Ruby propose quelques façons de s'attaquer à XML. REXML est une bibliothèque entièrement en Ruby qui est facile à aborder. Nokogiri est une gem qui enveloppe des bibliothèques C plus rapides, offrant vitesse et fonctionnalités supplémentaires. Entre les deux ? Commencez avec REXML pour des tâches plus petites et passez à Nokogiri si vous avez besoin de plus de puissance.

Sous le capot, analyser XML consiste à traduire des chaînes en modèles DOM ou SAX. DOM crée un arbre en mémoire, tandis que SAX diffuse le document et déclenche des événements au fur et à mesure de son analyse. REXML propose les deux modèles, mais a tendance à être plus lent que les extensions C utilisées par Nokogiri.

## Voir aussi :
- Documentation Ruby REXML : https://www.rubydoc.info/stdlib/rexml
- Gem Nokogiri : https://nokogiri.org/
- Spécification XML : https://www.w3.org/XML/
- Une introduction à SAX : https://www.saxproject.org/
- Comparaison entre YAML, JSON et XML : https://www.upwork.com/resources/json-vs-xml-vs-yaml
