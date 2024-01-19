---
title:                "Capitalizando una cadena de texto"
html_title:           "Gleam: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Capitalizar una cadena significa convertir la primera letra de esta a mayúscula. Los programadores lo hacen por razones estéticas o para resaltar ciertas palabras en un texto.

## ¿Cómo se hace?

En Gleam, capitalizamos una cadena utilizando la función `to_title` del módulo `string`.
 
```gleam
let mensaje = "hola mundo"
let mensajeTitulo = string.to_title(mensaje)
```

El código anterior convertirá "hola mundo" en "Hola Mundo".

## Vamos a profundizar 

1. **Contexto Histórico**: Las funciones para capitalizar cadenas tienen raíces en la tipografía y en la necesidad de destacar ciertos elementos textuales en documentos impresos. Hoy en día, en programación, se utilizan con frecuencia en interfaces de usuario, reportes, etc.

2. **Alternativas**: Además de `to_title`, Gleam ofrece otras funciones para trabajar con mayúsculas y minúsculas en cadenas, como `to_upper` y `to_lower`, que cambian todas las letras de la cadena a mayúsculas o minúsculas respectivamente. 

3. **Detalles de Implementación**: La función `to_title` de Gleam recorre la cadena de caracteres y, cada vez que encuentra un carácter de espacio seguido de una letra, convierte esa letra a mayúscula.

## Ver También

Para obtener más información sobre la manipulación de cadenas en Gleam, te recomendamos visitar:

1. [Gleam string module documentation](https://hexdocs.pm/gleam_stdlib/gleam/string/)
2. [Capitalization in different programming languages](https://en.wikipedia.org/wiki/Capitalization#Computing) for a broad view.