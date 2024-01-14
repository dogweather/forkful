---
title:                "Go: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Go?

Escribir pruebas es una práctica importante en cualquier lenguaje de programación, y Go no es una excepción. Las pruebas nos permiten verificar que nuestro código funciona correctamente y no ha sido afectado por cambios o actualizaciones. También nos ayuda a identificar y corregir posibles errores antes de que lleguen a producción.

## Cómo escribir pruebas en Go

En Go, las pruebas se realizan utilizando el paquete "testing" de la biblioteca estándar. Para comenzar, debemos crear un archivo de prueba con el sufijo "_test.go". Dentro de este archivo, podemos escribir funciones de prueba utilizando el prefijo "Test" seguido del nombre de la función que estamos probando. Por ejemplo:

```Go
package main

import "testing"

func TestSum(t *testing.T) {
  result := sum(2, 3)
  expected := 5
  if result != expected {
    t.Errorf("El resultado de suma esperado era %d, pero obtuvimos %d", expected, result)
  }
}

```

Aquí estamos probando una función "sum" que suma dos números enteros y verificamos que el resultado sea igual al esperado utilizando el método "t.Errorf" en caso contrario. Podemos ejecutar nuestras pruebas utilizando el comando "go test" en la terminal y ver los resultados.

## Profundizando en las pruebas

Además de las pruebas unitarias básicas como la que hemos visto anteriormente, Go también nos permite escribir pruebas de benchmarking y pruebas de integración. También podemos utilizar herramientas como "goconvey" para ejecutar nuestras pruebas de forma sencilla y visualizar los resultados. Es importante recordar que las pruebas también deben ser mantenidas y actualizadas junto con nuestro código.

## Ver también

- [Documentación de pruebas en Go](https://golang.org/pkg/testing/)
- [Tutorial de pruebas en Go](https://blog.golang.org/cover)
- [Escribiendo pruebas en Go: Ejemplos y prácticas recomendadas](https://medium.com/golang-eindhoven/writing-tests-in-go-examples-and-recommendations-7eac4c8fae4f)