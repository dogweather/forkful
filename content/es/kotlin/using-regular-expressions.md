---
title:                "Kotlin: Usando expresiones regulares"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Kotlin

Las expresiones regulares son una herramienta muy útil en programación, ya que nos permiten buscar patrones específicos en textos y reemplazarlos de manera eficiente. En Kotlin, también podemos utilizarlas para validar datos de entrada y realizar operaciones de manipulación de cadenas. ¡Sigue leyendo para aprender cómo utilizarlas en tus proyectos de Kotlin!

## Cómo utilizar expresiones regulares en Kotlin

Para utilizar expresiones regulares en Kotlin, primero debemos importar la clase Regex. Luego, podemos utilizar el método "matches" para verificar si una cadena cumple con el patrón buscado. Por ejemplo, si queremos encontrar si una cadena contiene solo números, podemos usar lo siguiente:

```Kotlin
val regex = Regex("[0-9]+") // declaramos la expresión regular
val string = "123" // cadena de ejemplo
if(string.matches(regex)) { // verificamos si la cadena coincide con la expresión regular
    println("La cadena contiene solo números")
} else {
    println("La cadena no cumple con el patrón")
}
```

También podemos utilizar el método "find" para encontrar todas las coincidencias con el patrón en una cadena. Por ejemplo, si queremos encontrar todas las direcciones de correo electrónico en una lista de strings, podemos hacer lo siguiente:

```Kotlin
val regex = Regex("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}") // expresión regular para validar un correo electrónico
val strings = listOf("ejemplo@correo.com", "otro_correo@hotmail.com", "invalido.com") // lista de strings de ejemplo
val emails = strings.filter { it -> regex.find(it) != null } // utilizamos el método "find" para obtener todas las coincidencias
println(emails) // imprimimos las direcciones de correo electrónico válidas
```

## Profundizando en el uso de expresiones regulares en Kotlin

En Kotlin, al igual que en la mayoría de lenguajes de programación, las expresiones regulares utilizan metacaracteres para representar patrones específicos. Algunos de los más comunes son: 

- "+" para indicar uno o más caracteres de un conjunto
- "*" para indicar cero o más caracteres de un conjunto
- "?" para indicar cero o uno de un conjunto
- "." para representar cualquier caracter
- "^" para indicar el inicio de una cadena
- "$" para indicar el fin de una cadena

Además, en Kotlin podemos utilizar "grupos de captura" para guardar partes específicas de una cadena que coinciden con un patrón. Por ejemplo, si queremos extraer el nombre de usuario de una dirección de correo electrónico, podemos utilizar lo siguiente:

```Kotlin
val regex = Regex("[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\\.[A-Za-z]{2,4})") // expresión regular con un grupo de captura en los paréntesis
val email = "ejemplo@correo.com" // dirección de correo electrónico de ejemplo
val match = regex.find(email) // buscamos la coincidencia en la cadena
val username = match?.groupValues?.get(1) // obtenemos el grupo de captura correspondiente al nombre de usuario
println(username) // imprimimos el nombre de usuario
```

Recuerda que puedes experimentar con diferentes patrones y utilizar herramientas en línea como [Regex101](https://regex101.com/) para probar y comprender mejor el funcionamiento de las expresiones regulares en Kotlin.

## Ver también

- [Documentación oficial de Kotlin sobre expresiones regulares](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutorial de expresiones regulares en Kotlin](https://www.baeldung.com/kotlin/regex)
- [Ejemplos prácticos de uso de expresiones regulares en Kotlin](https://medium.com/@imwower/examples-of-regular-expressions-regex-in-kotlin-c0b5bd29dc4)