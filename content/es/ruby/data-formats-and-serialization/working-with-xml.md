---
date: 2024-01-26 04:35:10.280807-07:00
description: "Trabajar con XML significa analizar, generar y manipular documentos\
  \ XML (eXtensible Markup Language) utilizando c\xF3digo. Los programadores lo hacen\
  \ para\u2026"
lastmod: '2024-03-13T22:44:59.615228-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con XML significa analizar, generar y manipular documentos XML\
  \ (eXtensible Markup Language) utilizando c\xF3digo."
title: Trabajando con XML
weight: 40
---

## ¿Qué y por qué?
Trabajar con XML significa analizar, generar y manipular documentos XML (eXtensible Markup Language) utilizando código. Los programadores lo hacen para interactuar con muchos servicios web, archivos de configuración e intercambios de datos donde XML es la lengua franca.

## Cómo hacerlo:
Usaremos REXML, incluido con Ruby, para analizar un fragmento de XML:
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
document.elements.each('fruits/fruit') { |element|
  puts "Nombre: #{element.attributes['name']}, Color: #{element.attributes['color']}"
}
```
Salida:
```
Nombre: apple, Color: green
Nombre: banana, Color: yellow
```

Generar XML también es sencillo:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Salida XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Estudio detallado:
Las raíces de XML se remontan a los años 90 como un subconjunto simplificado de SGML para documentos web. Es verboso pero altamente estructurado, y es por eso que ha perdurado. No es el único método existente: JSON y YAML se han popularizado por su simplicidad, pero XML se mantiene firme en muchos sistemas empresariales y legados.

Ruby ofrece varias formas de abordar XML. REXML es una biblioteca completamente en Ruby que es fácil de comenzar a usar. Nokogiri es una gema que envuelve bibliotecas de C más rápidas, ofreciendo velocidad y características adicionales. ¿Decidir entre ellas? Comienza con REXML para tareas más pequeñas y pasa a Nokogiri si necesitas más potencia.

Por debajo, analizar XML se trata de traducir cadenas a modelos DOM o SAX. DOM crea un árbol en memoria, mientras que SAX transmite el documento y dispara eventos a medida que lo analiza. REXML ofrece ambos modelos, pero tiende a ser más lento que las extensiones de C como las que utiliza Nokogiri.

## Ver también:
- Documentación de Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Gema Nokogiri: https://nokogiri.org/
- Especificación XML: https://www.w3.org/XML/
- Una introducción a SAX: https://www.saxproject.org/
- Comparación entre YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
