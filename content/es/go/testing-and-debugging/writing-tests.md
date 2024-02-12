---
title:                "Escribir pruebas"
aliases:
- /es/go/writing-tests/
date:                  2024-02-03T18:15:32.977797-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribir pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en Go implica crear pequeñas piezas de código manejables que validan la funcionalidad y el comportamiento de tu aplicación. Los programadores escriben pruebas para asegurarse de que su código funciona como se espera bajo diversas condiciones, para facilitar la refactorización y para ayudar a prevenir regresiones.

## Cómo hacerlo:

En Go, las pruebas se escriben típicamente en el mismo paquete que el código que prueban. Los archivos que contienen las pruebas tienen el sufijo `_test.go`. Las pruebas son funciones que toman un puntero al objeto testing.T (del paquete `testing`) como argumento, y señalan un fallo llamando a métodos como `t.Fail()`, `t.Errorf()`, etc.

Ejemplo de una prueba simple para una función `Add` definida en `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Archivo de prueba `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; se esperaba %d", result, expected)
    }
}
```

Ejecuta tus pruebas con el comando `go test` en el mismo directorio que tus archivos de prueba. Un ejemplo de salida que indica una prueba superada sería similar a:

```
PASS
ok      example.com/my/math 0.002s
```

Para pruebas impulsadas por tablas, que te permiten probar de manera eficiente diversas combinaciones de entrada y salida, define un slice de structs que representan los casos de prueba:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("obtuvo %d, se esperaba %d", ans, tt.expected)
            }
        })
    }
}
```

## Análisis Detallado

El marco de pruebas de Go, introducido en Go 1 junto con el lenguaje mismo, fue diseñado para integrarse a la perfección con la cadena de herramientas de Go, reflejando el énfasis de Go en la simplicidad y eficiencia en el desarrollo de software. A diferencia de algunos marcos de pruebas en otros idiomas que dependen de bibliotecas externas o configuraciones complejas, el paquete `testing` integrado de Go ofrece una forma sencilla de escribir y ejecutar pruebas.

Un aspecto interesante del enfoque de Go para las pruebas es el principio de convención sobre configuración que adopta, como el patrón de nombramiento de archivos (`_test.go`) y el uso de funcionalidades de la biblioteca estándar sobre dependencias externas. Este enfoque minimalista alienta a los desarrolladores a escribir pruebas, ya que la barrera de entrada es baja.

Aunque las instalaciones de prueba integradas de Go cubren mucho terreno, hay escenarios donde herramientas o marcos de terceros podrían ofrecer más funcionalidades, como generación de simulacros, pruebas de fuzzing o pruebas de estilo de desarrollo guiado por comportamiento (BDD). Bibliotecas populares como Testify o GoMock complementan las capacidades de prueba estándar de Go, ofreciendo afirmaciones más expresivas o capacidades de generación de simulacros, que pueden ser particularmente útiles en aplicaciones complejas con muchas dependencias.

A pesar de la existencia de estas alternativas, el paquete de pruebas estándar de Go sigue siendo la piedra angular para las pruebas en Go debido a su simplicidad, rendimiento y estrecha integración con el lenguaje y la cadena de herramientas. Ya sea que los desarrolladores elijan complementarlo con herramientas de terceros o no, el marco de pruebas de Go proporciona una base sólida para garantizar la calidad y fiabilidad del código.
