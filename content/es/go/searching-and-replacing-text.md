---
title:    "Go: Buscando y reemplazando texto"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en la tarea de reemplazar un texto en tu programa de Go? Quizás necesites cambiar una palabra específica en una cadena de texto o actualizar el nombre de una variable en todo tu código. Sea cual sea el caso, buscar y reemplazar texto es una habilidad esencial que te ahorrará tiempo y esfuerzo en el proceso de programación.

## Cómo

El proceso para buscar y reemplazar texto en Go es sencillo y fácil de dominar. Primero, necesitamos importar el paquete `strings` en nuestro programa. Luego, usamos la función `ReplaceAll` proporcionada por este paquete para buscar y reemplazar texto en una cadena.

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    texto := "¡Hola Mundo!"
    nuevoTexto := strings.ReplaceAll(texto, "Hola", "Adiós")
    fmt.Println(nuevoTexto)
}

```

El código anterior imprimirá "¡Adiós Mundo!" en la consola. Como puedes ver, hemos utilizado la función `ReplaceAll` para buscar el texto "Hola" en nuestra variable `texto` y reemplazarlo con "Adiós".

También podemos reemplazar texto en una cadena utilizando expresiones regulares. Por ejemplo:

```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    texto := "perro gato oso"
    patron := regexp.MustCompile("perro")
    nuevoTexto := patron.ReplaceAllString(texto, "conejo")
    fmt.Println(nuevoTexto)
}
```

Este código imprimirá "conejo gato oso" en la consola.

## Profundizando

Ahora que ya sabemos cómo utilizar la función `ReplaceAll` y las expresiones regulares para buscar y reemplazar texto en Go, podemos explorar más a fondo las diversas opciones y métodos que nos proporciona el paquete `strings`. Por ejemplo, podemos utilizar las funciones `Replace` y `ReplaceAll` para reemplazar sólo una cierta cantidad de ocurrencias de un texto en una cadena. También podemos utilizar las funciones `ToLower` y `ToUpper` para convertir texto a minúsculas y mayúsculas, respectivamente.

Otra característica interesante del paquete `strings` es la función `Split`, que nos permite dividir una cadena en varias subcadenas utilizando un separador específico. Esto puede ser útil para procesar datos en formato CSV o para dividir una gran cadena en segmentos más pequeños.

## Ver también

- [Documentación oficial del paquete strings en Go](https://golang.org/pkg/strings/)
- [Uso de expresiones regulares en Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-es)
- [Explicación más detallada sobre el paquete strings en Go](https://www.calhoun.io/what-the-stdlib-part-1-strings/)