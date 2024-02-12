---
title:                "Capitalizando una cadena de texto"
aliases:
- /es/powershell/capitalizing-a-string.md
date:                  2024-02-03T19:05:56.421753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena en PowerShell implica transformar el primer carácter de una cadena dada a mayúscula, mientras que el resto de la cadena permanece sin cambios. Los programadores a menudo realizan esta tarea con fines de formato, como preparar texto para su visualización en interfaces de usuario o seguir reglas gramaticales en documentos generados.

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
