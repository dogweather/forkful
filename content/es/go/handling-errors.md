---
title:                "Manejo de errores"
date:                  2024-01-26T00:52:29.436360-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El manejo de errores en Go consiste en capturar y responder con elegancia a los contratiempos en tiempo de ejecución. Lo hacemos para prevenir caídas y asegurar que nuestros programas actúen de manera predecible, incluso cuando las cosas van mal.

## Cómo hacerlo:

Go utiliza el manejo de errores explícito. Eso significa que comprobarás si una función retorna un error cada vez que la llamas. Sin excepciones. Así es como se ve:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := hacerAlgo()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func hacerAlgo() error {
	// Fingiendo que algo salió mal
	return fmt.Errorf("algo salió mal")
}
```

Ejecuta esto, y obtendrás:

```
Uh oh: algo salió mal
```

¿Pero qué pasa si tiene éxito?

```Go
func hacerAlgo() error {
	// Todo bien esta vez
	return nil
}
```

Sin salida. Genial, no hay noticias buenas noticias.

## Profundización:

En Go, el manejo de errores ha sido un punto de discusión. Desde el principio, Go decidió en contra de las excepciones por un enfoque más explícito, que algunos desarrolladores aman por su simplicidad y otros encuentran verboso. El tipo `error` incorporado es una interfaz. Cualquier tipo con un método `Error() string` lo satisface. Esto está en línea con la ética de Go de simplicidad y explicitud.

¿Alternativas? Están el dúo `panic` y `recover`, pero son para casos excepcionales (juego de palabras) cuando el programa no puede continuar. Piensa en `panic` como el botón de expulsión que presionas cuando sabes que no hay vuelta atrás. Úsalo con moderación.

En cuanto al manejo de errores convencional, Go 1.13 introdujo el envoltorio de errores, facilitando el desciframiento de la "cadena de errores" con funciones como `errors.Is()` y `errors.As()`.

## Ver También:

Para todo sobre manejo de errores en Go:

- El Blog de Go sobre el Manejo de Errores: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Go Eficaz – Sección de manejo de errores: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Documentación sobre el Envoltorio de Errores de Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Publicación de Dave Cheney sobre estrategias de manejo de errores: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)