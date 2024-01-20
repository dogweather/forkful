---
title:                "Utilizando expresiones regulares"
html_title:           "PowerShell: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué? 
Explicamos brevemente qué son las expresiones regulares y por qué los programadores las utilizan. 

Las expresiones regulares son patrones de texto utilizados para buscar y manipular cadenas de caracteres. Los programadores las utilizan para realizar operaciones de texto de manera más eficiente y precisa, en lugar de tener que escribir una gran cantidad de código para realizar la misma tarea. 

## ¿Cómo?: 
Ejemplos de código y salida de muestra dentro de los bloques de código ```PowerShell ...```

Para buscar un patrón específico en una cadena de texto, podemos usar el cmdlet ```Select-String```. Por ejemplo, si queremos encontrar todas las direcciones de correo electrónico en un archivo de texto, podemos usar el siguiente comando: 

```PowerShell 
Select-String -Path C:\ejemplo.txt -Pattern "\w+@\w+\.\w+" 
``` 

Esto devolverá todas las direcciones de correo electrónico que coincidan con el patrón en el archivo de texto. Podemos usar expresiones regulares para una variedad de tareas, como validación de entradas de usuario, extracción de información específica de un texto y reemplazo de cadenas de caracteres.

## Profundizando:
Información adicional sobre el contexto histórico, alternativas y detalles de implementación de expresiones regulares en PowerShell. 

Las expresiones regulares han estado presentes en lenguajes de programación desde la década de 1950, pero se volvieron más populares en la década de 1990 con la aparición de Perl y su compatibilidad incorporada con expresiones regulares. En PowerShell, las expresiones regulares se pueden utilizar en cmdlets como ```Select-String```, ```Where-Object``` y ```ForEach-Object```. También se pueden usar para la validación de parámetros en funciones y scripts.

En lugar de expresiones regulares, se pueden utilizar otras técnicas, como las funciones de búsqueda y reemplazo de cadenas de caracteres en PowerShell, aunque no son tan poderosas y versátiles como las expresiones regulares.

## Vea también:
Enlaces a fuentes relacionadas con el uso de expresiones regulares en PowerShell. 

- [Documentación de Microsoft sobre Expressions regulares en PowerShell](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7)
- [Tutorial en YouTube sobre expresiones regulares en PowerShell por LearnWinAutomation](https://www.youtube.com/watch?v=xX3c-kQ94Sg)