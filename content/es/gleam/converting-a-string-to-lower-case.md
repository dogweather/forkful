---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Convertir una cadena a minúsculas implica cambiar todos los caracteres en mayúscula de una cadena a su correspondiente formato en minúscula. Los programadores hacen esto para normalizar los datos de entrada y realizar comparaciones insensibles a las mayúsculas.

## ¿Cómo se hace?
Con Gleam, utilizamos la función `string.to_lower`. Veamos un ejemplo.

```Gleam
import gleam/string

fn cadena_a_minuscula() {
  let cadena = "Hola Mundo"
  string.to_lower(cadena)
}
```

Ejecutando este código obtendrás "hola mundo" como resultado.

## Profundizando 
La conversión de cadenas a minúsculas viene desde los primeros días de la informática, cuando se necesitaba una forma de hacer las comparaciones de cadenas independientes del formato de las mismas.

Una alternativa a `string.to_lower` podría ser hacer tu propia función usando `string.foldl`.

En cuanto a cómo Gleam implementa `string.to_lower`, se utiliza el módulo de Erlang underlying's, que tiene una implementación eficiente y segura en términos de localización y Unicode.

## Ver también
Para más sobre el manejo de cadenas en Gleam, revisa los siguientes recursos:

1. Documentación oficial de Gleam para el módulo string: https://hexdocs.pm/gleam_stdlib/gleam/string/
2. Tutorial sobre manipulación de strings en Gleam: https://gleam.run/book/tour/strings