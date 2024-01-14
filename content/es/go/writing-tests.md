---
title:                "Go: Escribiendo pruebas"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué
Escribir pruebas puede parecer una tarea tediosa e innecesaria para algunos programadores, pero en realidad es una práctica muy útil para mejorar la calidad de nuestro código. Las pruebas nos permiten detectar errores y fallos de manera temprana, lo que ahorra tiempo y esfuerzo en el futuro. Además, nos da una mayor confianza en nuestro código y nos permite realizar cambios sin preocuparnos por romper funcionalidades existentes. En pocas palabras, escribir pruebas hace que nuestro código sea más robusto y confiable.

## Cómo hacerlo
Para escribir pruebas en Go, utilizamos el paquete incorporado "testing", que proporciona una serie de funciones y métodos para crear y ejecutar pruebas. Veamos un ejemplo simple de cómo escribir y ejecutar una prueba en Go:

```Go
// Creamos una función simple para sumar dos números
func sum(a, b int) int {
  return a + b
}

// Definimos una prueba para nuestra función sum
func TestSum(t *testing.T) {
  // Llamamos a la función sum y almacenamos el resultado en una variable
  result := sum(2, 3)
  // Definimos el resultado esperado
  expected := 5
  // Comparamos el resultado con el esperado utilizando el método "Errorf" de testing
  if result != expected {
    t.Errorf("El resultado fue incorrecto, esperado: %v, obtenido: %v", expected, result)
  }
}
```

El código anterior muestra cómo crear una función de prueba utilizando el paquete "testing" y cómo comparar el resultado con el valor esperado utilizando el método "Errorf". Para ejecutar todas las pruebas en un paquete, solo necesitamos correr el comando "go test" en la terminal.

## Profundizando
Es importante destacar que las pruebas en Go siguen el principio de "prueba única", lo que significa que cada función o método debe tener una prueba correspondiente. También es recomendable utilizar el paquete "testing/quick" para generar pruebas aleatorias y detectar posibles casos extremos. Además, Go nos permite crear pruebas de rendimiento y de cobertura para evaluar el desempeño y la calidad de nuestro código.

Es crucial tener en cuenta que las pruebas son solo una herramienta para garantizar la calidad de nuestro código, pero no reemplazan la necesidad de un buen diseño y programación. Por lo tanto, es importante escribir pruebas significativas que cubran los casos más importantes y no solo pruebas para lograr una cobertura del 100%.

## Ver también
- [Paquete de prueba "testing" en la documentación oficial de Go](https://golang.org/pkg/testing/)
- [Tutorial de pruebas en "godoc.org"](https://godoc.org/testing)
- [Artículo sobre mejores prácticas para escribir pruebas en Go](https://medium.com/@pierreprinetti/best-practices-when-writing-go-test-code-7fdb7589ff17)