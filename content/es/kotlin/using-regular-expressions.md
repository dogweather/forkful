---
title:    "Kotlin: Utilizando expresiones regulares"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Kotlin

Las expresiones regulares son patrones de búsqueda que permiten encontrar y manipular texto de manera eficiente. Al usar expresiones regulares en Kotlin, puedes ahorrar tiempo y mejorar la precisión de tus búsquedas de texto en tus programas de una manera sencilla y efectiva.

## Cómo utilizar expresiones regulares en Kotlin

Para utilizar expresiones regulares en Kotlin, es necesario importar la clase "Regex" y utilizar su método "match" para especificar el patrón a buscar. Por ejemplo, si queremos buscar una cadena que contenga solo letras y números, podemos utilizar la siguiente expresión regular:

```Kotlin
val regex = Regex("[a-zA-Z0-9]+")
val input = "Hola123"
println(input.contains(regex)) // output: true
```

En este ejemplo, la expresión regular "[a-zA-Z0-9]+" indica que estamos buscando una cadena que contenga cualquier combinación de letras mayúsculas o minúsculas y números. Al utilizar el método "contains" en la variable "input", nos devuelve un valor booleano que indica si la cadena cumple con el patrón o no.

Otro ejemplo común es el uso de la expresión regular "^[a-zA-Z]{5}$", que busca una cadena de exactamente 5 letras. Si utilizamos esta expresión en una declaración "if", podemos asegurarnos de que solo se acepten contraseñas de 5 letras en nuestro programa:

```Kotlin
val regex = Regex("^[a-zA-Z]{5}$")
val password = "Hola"
if(password.matches(regex)){
    println("Contraseña aceptada")
}else{
    println("Contraseña no válida")
} // output: Contraseña no válida
```

## Profundizando en el uso de expresiones regulares en Kotlin

Además de los ejemplos anteriores, las expresiones regulares en Kotlin permiten una amplia variedad de patrones y funciones para manipular y buscar texto. Algunas de las funciones más comunes incluyen "find", que devuelve el primer resultado que cumpla con el patrón, "replace", que permite reemplazar partes de una cadena y "split", que divide una cadena en una lista según el patrón especificado.

Puedes experimentar con diferentes patrones y funciones para encontrar la combinación que mejor se adapte a tus necesidades. Además, existen numerosas librerías en línea que ofrecen expresiones regulares ya creadas para diversas tareas, lo cual puede ser de gran ayuda para simplificar el proceso de búsqueda de patrones complejos.

## Consulta también

- [Expresiones regulares en Kotlin](https://kotlinlang.org/docs/regex.html)
- [Guía rápida de uso de expresiones regulares en Kotlin](https://dev.to/asantono/kotlin-regex-quick-programmer-guide-4fnk)
- [Expresiones regulares: una herramienta poderosa para manipular texto](https://opensource.com/article/19/3/power-regular-expressions) (en inglés)