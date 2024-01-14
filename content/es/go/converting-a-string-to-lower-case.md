---
title:    "Go: Convirtiendo una cadena a minúsculas."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Go?

Convertir una cadena a minúsculas es una tarea común en muchas aplicaciones de programación. En Go, esta funcionalidad se logra fácilmente gracias a la librería incorporada "strings". En este artículo, aprenderás cómo convertir una cadena a minúsculas en Go y por qué es una habilidad útil para tener en tu caja de herramientas de programación.

## Cómo hacerlo

Hay varias formas de convertir una cadena a minúsculas en Go. Una forma es utilizando el método "ToLower" de la librería "strings". Aquí está un ejemplo de cómo se vería esto en código:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    cadena := "PROGRAMANDO EN GO"
    cadenaMin := strings.ToLower(cadena)
    fmt.Println(cadenaMin)
}
```
La salida de este código sería: "programando en go". Como puedes ver, el método "ToLower" convierte toda la cadena a minúsculas.

Otra opción para convertir una cadena a minúsculas es utilizando el método "ToLowerSpecial" de la librería "unicode". Este método te permite especificar el idioma que quieres utilizar para la conversión, lo que puede ser útil en situaciones multilingües. Aquí está un ejemplo de cómo se vería esto en código:

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    cadena := "PROGRAMANDO EN GO"
    cadenaMin := strings.ToLowerSpecial(unicode.TurkishCase, cadena)
    fmt.Println(cadenaMin)
}
```
La salida de este código sería: "programando en go", pero con la letra "i" en minúscula y sin el acento que se encuentra en el idioma turco.

## Profundizando

Ahora que ya sabes cómo convertir una cadena a minúsculas en Go, es importante entender cómo funciona este proceso. La librería "strings" y el método "ToLower" utilizan la tabla "ASCII" para realizar la conversión. Esta tabla contiene pares de valores para cada letra, por lo que el método simplemente busca la letra en mayúscula y la reemplaza con la letra en minúscula correspondiente.

Por otro lado, el método "ToLowerSpecial" utiliza la tabla "Unicode" para la conversión. Esta tabla contiene más de 128,000 caracteres y es utilizada para soportar diferentes idiomas y símbolos. Al especificar un idioma en el método, estás indicando a Go que utilice la tabla "Unicode" correspondiente a ese idioma para realizar la conversión.

## Ver también

Si quieres aprender más sobre Go y sus funcionalidades, aquí hay algunos recursos que pueden serte útiles:

- [Documentación oficial de Go](https://golang.org/doc/)
- [Videos tutoriales sobre Go](https://www.youtube.com/playlist?list=PLWDnPyr5QZxMpkGIWNuQLPAw1gPoid3Ue)
- [Comunidad de Go en español](https://golang.es/)

¡Esperamos que este artículo te haya sido útil y te haya ayudado a aprender más sobre Go y su funcionalidad para convertir cadenas a minúsculas!