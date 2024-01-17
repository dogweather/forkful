---
title:                "Utilizando expresiones regulares"
html_title:           "Elm: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares son una herramienta poderosa para buscar y manipular patrones de texto en programas informáticos. Los programadores las utilizan para validar entradas de usuario, buscar y reemplazar texto en grandes documentos y realizar tareas de procesamiento de texto. 

## Cómo: 
Usar expresiones regulares en Elm es fácil gracias al paquete Regex incorporado en el lenguaje. Para comenzar, simplemente importa el paquete en tu código y usa la función "Regex.find" para buscar un patrón específico en una cadena de texto. A continuación, se muestra un ejemplo básico de cómo encontrar una dirección de correo electrónico en una cadena de texto y mostrarla en la consola:

```Elm
import Regex

texto = "Mi dirección de correo electrónico es user@domain.com"

Regex.find "([a-z]+@[a-z]+\\.[a-z]{3,})" texto
--> Just "user@domain.com"
```

## Deep Dive:
Las expresiones regulares tienen su origen en los años 50 y han evolucionado a lo largo del tiempo en diferentes versiones y sabores. Además de su uso en Elm, también se pueden encontrar en otros lenguajes de programación como JavaScript y Python. Aunque son una herramienta poderosa, pueden ser complicadas de entender y de escribir, especialmente para patrones complejos. En su lugar, se pueden utilizar bibliotecas que simplifiquen su uso, como "elm-regex-builder", que proporciona una interfaz visual para construir expresiones regulares de manera más intuitiva.

## Ver también:
- Documentación oficial de Regex en Elm: https://package.elm-lang.org/packages/elm/regex/latest/
- "elm-regex-builder": https://package.elm-lang.org/packages/joneshf/elm-regex-builder/latest/