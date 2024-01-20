---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas es realizar código que verifica la correctitud de otros códigos. Los programadores lo hacen para asegurarse de que sus programas funcionen como se espera y para prevenir errores futuros.

## Cómo:

Para comenzar a escribir pruebas en Go usaremos el paquete `testing`. Observa este código sencillo de prueba para una función que suma dos números:

```Go
package suma

// Suma dos enteros y devuelve el resultado.
func Suma(x, y int) int {
    return x + y
}
```
La prueba para la función `Suma` podría verse así:

```Go
package suma

import (
    "testing"
    "fmt"
)

func TestSuma(t *testing.T) {
    resultado := Suma(5, 5)
    if resultado != 10 {
        t.Errorf("Suma(5, 5) = %d; want 10", resultado)
    } else {
      fmt.Printf("TestSuma: éxito\n")
    }
}
```
Para ejecutar la prueba, ejecuta `go test` en la terminal. Si todo está correcto, verás algo así:

```
ok      suma    0.001s
```
Si hay un fallo, se mostrará un mensaje detallando el error.

## Exploración Profunda

Existen frameworks más avanzados y completos además del paquete `testing`, como `GoConvey` o `Testify`. En términos históricos, las pruebas de software se convirtieron en una mejor práctica conforme los sistemas se volvieron más complejos. A nivel implementación, Go ejecuta las pruebas en paralelo por defecto, lo cual hace que el proceso sea eficiente pero obliga a las pruebas a ser seguras para su ejecución concurrente.

## Ver También

- Documentación oficial de Go para pruebas: [Go Testing](https://golang.org/pkg/testing/)
- GoConvey framework: [GoConvey on GitHub](https://github.com/smartystreets/goconvey)
- Testify para aserciones más expresivas: [Testify on GitHub](https://github.com/stretchr/testify)