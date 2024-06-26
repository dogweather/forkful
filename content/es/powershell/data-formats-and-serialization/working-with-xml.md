---
date: 2024-01-26 04:34:16.884464-07:00
description: "C\xF3mo: XML, o lenguaje de marcado eXtensible, existe desde finales\
  \ de los '90 y sigue siendo un formato ampliamente utilizado para datos estructurados.\u2026"
lastmod: '2024-04-05T22:51:13.039504-06:00'
model: gpt-4-0125-preview
summary: XML, o lenguaje de marcado eXtensible, existe desde finales de los '90 y
  sigue siendo un formato ampliamente utilizado para datos estructurados.
title: Trabajando con XML
weight: 40
---

## Cómo:
```PowerShell
# Cargando un archivo XML en una variable
[xml]$xmlContent = Get-Content 'ruta\a\tu\archivo.xml'

# Accediendo a nodos XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Título: $($book.title)"
}

# Creando un nuevo elemento XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Guardando el XML de vuelta al archivo
$xmlContent.Save('ruta\a\tu\archivo\actualizado.xml')
```
Salida de muestra:
```
Título: Programación en PowerShell
Título: Esenciales de XML
```

## Estudio Profundo
XML, o lenguaje de marcado eXtensible, existe desde finales de los '90 y sigue siendo un formato ampliamente utilizado para datos estructurados. PowerShell simplifica trabajar con XML en comparación con los métodos de análisis tradicionales; convierte el XML en objetos directamente, permitiéndote interactuar con los elementos a través de la notación de punto familiar.

Alternativas a XML incluyen JSON, YAML o formatos de datos personalizados. JSON, por ejemplo, ha ganado popularidad por su naturaleza ligera y facilidad de uso con tecnologías web. Sin embargo, las características extendidas de XML como espacios de nombres, esquemas y procesamiento XSLT a menudo lo hacen más adecuado para documentos complejos o estándares de la industria.

PowerShell utiliza las capacidades de XML del .NET Framework para su manejo de XML. Esto significa que no se trata solo de operaciones simples de lectura-escritura; también puedes trabajar con esquemas XML para la validación, usar XPath para consultas y emplear transformaciones XSLT, todo a través de PowerShell.

## Ver También
- [Tutorial de XML de W3Schools](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
