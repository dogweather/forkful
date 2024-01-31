---
title:                "Concatenación de cadenas de texto"
date:                  2024-01-20T17:34:48.009170-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Concatenar cadenas es simplemente juntar dos o más strings en uno solo. Programadores lo hacen todo el tiempo para manipular texto: crear mensajes, generar URLs, o combinar datos para la salida.

## Cómo hacerlo:

En Go, concatenar es pan comido. Podés usar el operador `+` o la función `fmt.Sprintf`. Aquí unos ejemplos:

```Go
package main

import (
	"fmt"
)

func main() {
	// Concatenación simple con el operador '+'
	hola := "Hola"
	mundo := "Mundo"
	holaMundo := hola + " " + mundo
	fmt.Println(holaMundo) // Salida: Hola Mundo

	// Concatenación con fmt.Sprintf
	holaMundoSprintf := fmt.Sprintf("%s %s", hola, mundo)
	fmt.Println(holaMundoSprintf) // Salida: Hola Mundo
}
```

## Profundización

Historicamente, la concatenación era un poco más compleja, especialmente en lenguajes de bajo nivel donde tenías que manejar la memoria manualmente. En Go, es sencillo gracias a su potente estándar de librerías y al recolector de basura automático. Si querés eficiencia, especialmente en bucles o concatenaciones masivas, considerá usar `strings.Builder`:

```Go
package main

import (
	"strings"
	"fmt"
)

func main() {
	var builder strings.Builder
	for i := 0; i < 10; i++ {
		builder.WriteString(fmt.Sprintf("%d...", i))
	}
	fmt.Println(builder.String()) // Salida: 0...1...2...3...4...5...6...7...8...9...
}
```

`strings.Join` también es una alternativa eficiente, especialmente cuando tienes un slice de strings:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	palabras := []string{"¿Qué", "onda", "viejo?"}
	frase := strings.Join(palabras, " ")
	fmt.Println(frase) // Salida: ¿Qué onda viejo?
}
```

Recordá que la eficiencia importa cuando trabajás con grandes volúmenes de datos o en aplicaciones de alto rendimiento.

## Ver También

- Documentación oficial de Go para `strings` package: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
- Go blog sobre strings: [https://blog.golang.org/strings](https://blog.golang.org/strings)
- Tutorial de Go: Manipular strings: [https://tour.golang.org/moretypes/15](https://tour.golang.org/moretypes/15)
