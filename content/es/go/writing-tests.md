---
title:    "Go: Escribiendo pruebas"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas es una parte esencial del proceso de desarrollo de software en Go. Las pruebas aseguran que nuestro código funcione correctamente y nos permiten realizar cambios de manera segura sin preocuparnos por posibles errores. Además, las pruebas nos ayudan a documentar nuestro código y a comprender mejor su funcionamiento.

## Cómo hacerlo

Para escribir pruebas en Go, utilizamos el paquete de pruebas estándar "testing". Primero, creamos un archivo de prueba con una función de prueba y luego la ejecutamos utilizando el comando "go test". Por ejemplo:

```Go
package main

import "testing"

func TestSum(t *testing.T) {
	result := sum(5, 10)
	if result != 15 {
		t.Errorf("La suma fue incorrecta, se esperaba 15 pero se obtuvo %d", result)
	}
}
```

Al ejecutar el comando "go test" en la terminal, debería mostrarse la salida: "PASS: TestSum" indicando que la prueba fue exitosa. Si el resultado de la prueba no es el esperado, se mostrará un mensaje de error indicando la diferencia.

También podemos utilizar la función "log" del paquete "testing" para imprimir mensajes de depuración durante la ejecución de la prueba. Esto es muy útil para entender qué está sucediendo en nuestro código y solucionar posibles errores.

## Profundizando en las pruebas

Existen diferentes tipos de pruebas en Go, como las pruebas unitarias, de integración y de aceptación. Cada una tiene un propósito y alcance diferentes, por lo que es importante comprender cuál es la mejor opción para nuestro caso en particular.

Las pruebas también pueden ayudarnos a seguir buenas prácticas de programación como escribir código modular y utilizando el principio de "una sola responsabilidad". Al escribir pruebas, nos forzamos a pensar en cómo nuestro código puede ser más fácil de probar y cómo hacerlo más robusto.

## Ver también

- Documentación oficial de pruebas en Go: https://golang.org/pkg/testing/
- Artículo sobre cómo escribir pruebas en Go: https://blog.golang.org/examples
- Ejemplos de código con pruebas en Go: https://github.com/golang/go/tree/master/src/testing