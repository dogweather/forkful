---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
La interpolación de cadenas es la incorporación de variables dentro de una cadena de texto. Los programadores lo hacen para manejar datos dinámicos en string fácilmente.

## ¿Cómo se hace?
El lenguaje de programación Elm no soporta directamente la interpolación de cadenas, aunque se puede lograr algo similar concatenando cadenas y variables utilizando el operador `++`.

Aquí tienes un ejemplo en Elm:

```Elm
nombre = "Juan"
saludo = "Hola, " ++ nombre ++ "!"
```

La salida de este código será: "Hola, Juan!"

## Inmersión Total
(1) Históricamente, Elm se diseñó para ser simple y predecible, evitando características que podrían causar confusiones y errores frecuentes. La interpolación de cadenas se ha interpretado como una de esas características. 

(2) Como alternativa a la concatenación, podría usar la función `toString` para convertir otros tipos de datos en cadenas que puedas unir fácilmente:

```Elm
edad = 30
mensaje = "Tu edad es " ++ toString edad
```

(3) ¿Cómo implementa Elm las cadenas? Elm representa las cadenas de texto como listas de caracteres. Los operadores utilizados para la concatenación simplemente unen estas listas.

## Ver También
Para obtener más información sobre las cadenas en Elm y métodos alternativos para usar datos dinámicos en una cadena, consulta los siguientes enlaces:

- [Guía oficial de Elm sobre cadenas](https://guide.elm-lang.org/types/string.html)
- [Documentación de Elm sobre el módulo String](https://package.elm-lang.org/packages/elm/core/latest/String) 

Recuerda siempre que cada lenguaje tiene sus propias peculiaridades y Elm no es una excepción. La ausencia de interpolación de cadena no disminuye su eficiencia para los fines para los que fue diseñado.