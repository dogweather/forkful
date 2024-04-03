---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.421753-07:00
description: "Capitalizar una cadena en PowerShell implica transformar el primer car\xE1\
  cter de una cadena dada a may\xFAscula, mientras que el resto de la cadena permanece\u2026"
lastmod: '2024-03-13T22:44:59.273248-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena en PowerShell implica transformar el primer car\xE1\
  cter de una cadena dada a may\xFAscula, mientras que el resto de la cadena permanece\
  \ sin cambios."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
PowerShell, siendo una herramienta versátil, te permite capitalizar una cadena utilizando métodos simples sin necesidad de bibliotecas de terceros. Aquí te mostramos cómo puedes hacerlo:

```powershell
# Usando el método integrado .Net 'ToTitleCase' de CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Salida:
```
Hello world
```

Nota: Este método capitaliza la primera letra de cada palabra. Si estrictamente deseas capitalizar solo la primera letra de la cadena y dejar el resto tal como está, podrías hacer algo como esto:

```powershell
# Capitalizando solo el primer carácter de una cadena
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Salida:
```
Hello world
```

PowerShell no incluye directamente una función simple para capitalizar solo la primera letra de una cadena, pero combinando los métodos básicos de manipulación de cadenas como `Substring(0,1).ToUpper()` y la concatenación, podemos lograr fácilmente el resultado deseado.
