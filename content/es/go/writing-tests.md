---
title:                "Escribiendo pruebas"
html_title:           "Go: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-tests.md"
---

{{< edit_this_page >}}

¡Hola programadores! ¿Quieres asegurar la calidad de tus programas en Go? ¡Entonces es hora de hablar sobre cómo escribir pruebas!

## ¿Qué y por qué?

Escribir pruebas es simplemente crear piezas de código que verifican si otra parte del código funciona correctamente. Los programadores hacen esto para asegurarse de que su código se comporte como se espera y para prevenir errores en el futuro.

## ¿Cómo?

Escribir pruebas en Go es muy sencillo. Primero, debes importar el paquete "testing". Luego, puedes escribir funciones de prueba que comienzan con "Test". Dentro de estas funciones, puedes utilizar la función "t.Fail()" para indicar si la prueba falló o no. A continuación, puedes ejecutar las pruebas con el comando "go test" en la terminal y verás los resultados de tus pruebas.

```Go
// Importar el paquete "testing"
import "testing"

// Función de prueba
func TestSum(t *testing.T) {
    // Lógica de prueba
    result := sum(5, 7)
    // Verificar si el resultado es correcto
    if result != 12 {
        t.Fail() // Prueba fallida
    }
}
```

## Profundizando

La idea de escribir pruebas no es nueva y ha existido en la programación desde hace mucho tiempo. En Go, la herramienta "go test" fue introducida en la versión 1.7 y ha sido ampliamente adoptada por los programadores para garantizar la calidad de sus programas.

Si bien escribir pruebas puede ser una tarea tediosa, es una práctica muy útil en el desarrollo de software. También existen otras herramientas en Go, como "Testify", que pueden ayudarte a escribir pruebas de manera más eficiente.

## Ver también

Si deseas obtener más información sobre cómo escribir pruebas en Go, te recomendamos estos recursos:

- [Documentación oficial de pruebas en Go](https://golang.org/pkg/testing/)
- [Artículo sobre pruebas en "Go Official Blog"](https://blog.golang.org/go15drs)
- [Paquete "Testify" para mejorar tus pruebas](https://github.com/stretchr/testify)