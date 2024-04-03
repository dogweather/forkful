---
date: 2024-01-20 15:33:21.585946-07:00
description: "C\xF3mo hacerlo: En PowerShell, puedes utilizar la librer\xEDa `HtmlAgilityPack`\
  \ para parsear HTML de manera sencilla. A continuaci\xF3n se muestra c\xF3mo instalar\u2026"
lastmod: '2024-03-13T22:44:59.290178-06:00'
model: unknown
summary: "En PowerShell, puedes utilizar la librer\xEDa `HtmlAgilityPack` para parsear\
  \ HTML de manera sencilla."
title: "An\xE1lisis de HTML"
weight: 43
---

## Cómo hacerlo:
En PowerShell, puedes utilizar la librería `HtmlAgilityPack` para parsear HTML de manera sencilla. A continuación se muestra cómo instalar la librería y un ejemplo de uso.

Instalación de HtmlAgilityPack mediante NuGet:
```PowerShell
Install-Package HtmlAgilityPack
```

Código para cargar un documento HTML y obtener títulos de un sitio web:
```PowerShell
# Cargar la librería
Add-Type -Path "ruta\a\HtmlAgilityPack.dll"

# Crea un nuevo objeto de HtmlWeb y carga la página
$htmlWeb = New-Object HtmlAgilityPack.HtmlWeb
$documento = $htmlWeb.Load("https://example.com")

# Extraer todos los elementos con la etiqueta 'h1'
$titulos = $documento.DocumentNode.SelectNodes("//h1")

# Mostrar los títulos
foreach ($titulo in $titulos) {
    Write-Host $titulo.InnerText
}
```

Resultado esperado (depende del contenido del sitio):
```
Bienvenidos a Example.com
Conoce más sobre nosotros
```

## Profundización:
**Contexto Histórico**: Anteriormente, la extracción de datos de HTML se hacía mediante expresiones regulares, pero esto resultaba complicado y propenso a errores debido a la naturaleza no lineal del HTML.

**Alternativas**: Aparte de `HtmlAgilityPack`, existen otras herramientas como `AngleSharp`, que es una librería moderna para .NET, y herramientas en otros lenguajes como BeautifulSoup para Python.

**Detalles de Implementación**: HtmlAgilityPack analiza el HTML en un DOM (Document Object Model) manejable, permitiendo consultas con XPath o LINQ para localizar y manipular nodos específicos del documento.

## Ver También:
- Documentación oficial de HtmlAgilityPack: [https://html-agility-pack.net/](https://html-agility-pack.net/)
- Referencia de XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
- GitHub de AngleSharp: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
