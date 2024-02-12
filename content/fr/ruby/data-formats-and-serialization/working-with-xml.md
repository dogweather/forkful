---
title:                "Travailler avec XML"
aliases:
- fr/ruby/working-with-xml.md
date:                  2024-01-26T04:35:11.070065-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec XML signifie analyser, générer et manipuler des documents XML (eXtensible Markup Language) en utilisant du code. Les programmeurs le font pour interagir avec de nombreux services web, fichiers de configuration et formats d'échange de données où l'XML est la lingua franca.

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
