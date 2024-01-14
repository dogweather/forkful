---
title:    "Go: Redacción de pruebas"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Go

Las pruebas son esenciales para garantizar la calidad y el funcionamiento correcto de nuestro código en Go. Además, las pruebas nos proporcionan una forma de verificar que nuestro programa trabaja como se espera y nos permiten detectar errores antes de que lleguen a producción.

## Cómo hacerlo

Para escribir pruebas efectivas en Go, debemos seguir algunos pasos clave. Primero, debemos asegurarnos de utilizar el paquete "testing" de Go para crear nuestra suite de pruebas. Luego, debemos escribir funciones de prueba que verifiquen aspectos específicos de nuestro código utilizando la función "t.Errorf()" para indicar un fallo si es necesario. Finalmente, podemos utilizar el comando "go test" para ejecutar nuestras pruebas y verificar si nuestro código pasa todos los casos de prueba.

```Go
package main

import "testing"

func Add(x, y int) int {
    return x + y
}

func TestAdd(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("La suma de 2 y 3 debería ser 5, pero recibimos %d", result)
    }
}
```

Al correr nuestras pruebas con el comando "go test", podemos ver que pasan exitosamente, lo que indica que nuestra función "Add" funciona correctamente. Si hacemos un cambio en nuestro código que afecte el resultado de la suma, nuestras pruebas también fallarán, lo que nos alerta sobre posibles errores en nuestro código.

## Profundizando

Escribir pruebas en Go no se trata solo de verificar que nuestro código produce el resultado correcto. También podemos utilizar las pruebas para medir la cobertura de nuestro código y asegurarnos de que todas las posibles rutas de ejecución están siendo cubiertas. Además, podemos escribir pruebas de benchmarking para medir la eficiencia de nuestro código y comparar diferentes implementaciones para mejorar el rendimiento.

El paquete "testing" de Go también ofrece otras funciones útiles, como "t.Helper()" para marcar una función de prueba como auxiliar y "t.Skip()" para saltar una prueba en particular. Conocer todas estas funciones nos permitirá escribir pruebas más robustas y efectivas.

# Ver también

- Documentación oficial de Go sobre pruebas: https://golang.org/pkg/testing/
- Tutorial de Go sobre pruebas: https://golang.org/doc/code.html#Testing
- Ejemplos de código de pruebas en Go: https://github.com/golang/go/wiki/LearnTesting