---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El análisis de HTML se trata de descomponer un documento HTML en sus elementos y atributos para manipularlos o extraer información. Los programadores lo hacen por muchas razones, desde la extracción de datos, la automatización de la web, hasta las pruebas de funcionalidad del software.

## ¿Cómo se hace?

Primero instalamos el módulo `HtmlAgilityPack` con `Install-Package`.

```PowerShell
Install-Package HtmlAgilityPack
```
Después, importamos el módulo y analizamos un archivo local de HTML.

```PowerShell
Add-Type -Path 'C:\path\to\HtmlAgilityPack.dll'
$doc = New-Object HtmlAgilityPack.HtmlDocument
$doc.Load('C:\path\to\your.html')
$doc.DocumentNode.SelectNodes('//a[@href]') | ForEach-Object { $_.Attributes['href'].Value }
```
O también podemos analizar HTML directamente desde una URL.

```PowerShell
$web = New-Object HtmlAgilityPack.HtmlWeb
$doc = $web.Load('https://example.com')
$doc.DocumentNode.SelectSingleNode('//title').InnerText
```

## Más a fondo

Historia: PowerShell comenzó incluyendo el análisis de HTML en su versión 3.0, aprovechando la biblioteca .NET `HtmlAgilityPack`. Antes de esto, los programadores solían hacer uso de métodos rudimentarios como el uso de expresiones regulares, lo cual era propenso a errores.

Alternativas: hay varias alternativas a PowerShell para analizar HTML, tales como Python con Beautiful Soup, Javascript con JSDOM o PHP con DOMDocument.

Detalles de la implementación: El `HtmlAgilityPack` en PowerShell permite a los programadores analizar HTML tanto bien formado como mal formado. Puede manejar tags que no están cerrados y las inconsistencias que son comunes en el HTML de la web real.

## Ver también

1. [Html agility pack](https://html-agility-pack.net/)
2. [Librerías de Python para parsear HTML](https://realpython.com/python-html-parser/#beautiful-soup)
3. [JSDOM en JavaScript](https://www.npmjs.com/package/jsdom)
4. [DOMDocument en PHP](https://php.net/manual/book.dom.php)