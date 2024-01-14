---
title:    "Gleam: Capitalizar una cadena de texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto puede ser útil al momento de presentar información de manera más ordenada y legible, especialmente en casos donde se requiere que la primera letra de cada palabra esté en mayúscula.

## Cómo hacerlo

En Gleam, capitalizar una cadena de texto es muy sencillo. Primero, debemos importar la función `capitalize` del módulo `String`:

```Gleam
import String

```

Luego, podemos utilizar la función `capitalize` en la cadena que deseamos capitalizar:

```Gleam
String.capitalize("hola mundo") // "Hola mundo"
String.capitalize("hola GLEAM") // "Hola Gleam"
```

Podemos ver que la función `capitalize` convierte la primera letra de la cadena en mayúscula y mantiene el resto de las letras en minúscula.

También podemos utilizar esta función en variables que contienen cadenas de texto:

```Gleam
let texto = "este es un ejemplo"
let texto_capitalizado = String.capitalize(texto) // "Este es un ejemplo"
```

## Profundizando

La función `capitalize` en Gleam utiliza la librería `unicode`, lo que significa que también puede capitalizar caracteres especiales y acentos en diferentes idiomas.

Otra alternativa para capitalizar una cadena de texto es utilizar la función `title_case` del módulo `String`. Esta función convierte todas las palabras de la cadena en mayúscula, incluyendo la primera letra.

```Gleam
String.title_case("hola mundo") // "Hola Mundo"
```

## Ver también

- [Documentación oficial de la función `capitalize`](https://gleam.run/core/String.html#fn-string.capitalize)
- [Ejemplos de la función `capitalize` en Gleam Playground](https://play.gleam.run/?code=import%20String%20string%20let%20us%20%3D%20%0A%20%20String.capitalize%20(%22hola%20mundo%22)&config=gleam-project)