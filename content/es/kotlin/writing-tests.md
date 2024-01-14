---
title:    "Kotlin: Escribiendo pruebas"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Kotlin

Escribir pruebas de código es una práctica esencial en el desarrollo de software. Aunque puede parecer tedioso y consumir tiempo extra, en realidad ahorra tiempo y problemas a largo plazo. En este artículo exploraremos por qué deberías considerar escribir pruebas en Kotlin para mejorar la calidad de tu código.

## Cómo escribir pruebas en Kotlin

Para escribir pruebas en Kotlin, puedes usar el framework de pruebas integrado en el lenguaje, llamado "JUnit". Aquí te dejamos un ejemplo de cómo escribir una prueba básica para una función que suma dos números:

```Kotlin
import org.junit.Test
import org.junit.Assert.*

class CalculadoraTest {

    @Test
    fun sumaDosNumeros() {
        val calculadora = Calculadora()
        val resultado = calculadora.sumar(2, 3)
        assertEquals(5, resultado)
    }
}

class Calculadora {
    fun sumar(a: Int, b: Int): Int {
        return a + b
    }
}

```

En el código de arriba, se importa el paquete "JUnit" para utilizar sus funciones de aserciones y se declara la clase "CalculadoraTest". Dentro de esta clase, se declara una función de prueba llamada "sumaDosNumeros", donde se crea una instancia de la clase "Calculadora" y se espera que la suma de dos números sea igual a 5. Luego, se declara la clase "Calculadora" donde se encuentra la función "sumar" que simplemente suma dos números.

Para ejecutar y ver el resultado de esta prueba en IntelliJ, simplemente haz clic derecho en el archivo y selecciona "Run 'CalculadoraTest'". Si todo está bien, deberías ver un resultado verde que indica "OK".

## Profundizando en las pruebas en Kotlin

Escribir pruebas en Kotlin puede ser muy beneficioso para tu código a largo plazo. No solo asegura que tu código funcione correctamente, sino que también actúa como documentación de tu código. Cuando escribes pruebas, estás obligado a pensar en todos los casos posibles y asegurarte de que tu función se comporte adecuadamente en cada uno de ellos. Esto ayuda a evitar errores y a detectarlos con mayor facilidad en futuras actualizaciones.

Además de utilizar JUnit, también puedes integrar otros frameworks de prueba, como "Mockito" para probar código que interactúa con bases de datos o servicios externos. También puedes utilizar "Robolectric" para realizar pruebas de interfaz de usuario en tus aplicaciones móviles.

Otra ventaja de escribir pruebas en Kotlin es que te permite manejar errores de una manera más eficiente. Si una prueba falla, puedes tener una idea de dónde está el problema y corregirlo rápidamente antes de que se convierta en un problema más grande en producción.

## Ver también

- [Documentación de JUnit](https://junit.org/junit5/docs/current/api/)
- [Documentación de Mockito](https://site.mockito.org/)
- [Documentación de Robolectric](http://robolectric.org/)

¡Esperamos que este artículo te haya convencido de la importancia de escribir pruebas en Kotlin y te motive a implementarlo en tus proyectos! Recuerda que la calidad del código no solo se mide por su funcionalidad, sino también por su capacidad de ser probado y mantenerse en el tiempo. ¡Empieza a escribir pruebas hoy mismo!