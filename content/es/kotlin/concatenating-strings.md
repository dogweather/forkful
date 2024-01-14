---
title:    "Kotlin: Uniendo cadenas de texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una tarea común en la programación, especialmente cuando se trabaja con datos de texto. Es una forma rápida y sencilla de combinar múltiples cadenas en una sola.

## Cómo hacerlo

La concatenación de cadenas en Kotlin se puede lograr de varias maneras, dependiendo del uso y la preferencia del programador. A continuación se presentan algunas formas comunes de hacerlo:

```Kotlin
val nombre = "Juan"
val apellido = "Pérez"
val nombreCompleto = nombre + " " + apellido
println(nombreCompleto)
```

Este ejemplo crea dos variables, "nombre" y "apellido", y luego las concatena para formar una tercera variable, "nombreCompleto", que se imprime en la consola como "Juan Pérez".

Se pueden utilizar también plantillas de cadenas para concatenar variables dentro de una cadena.

```Kotlin
val edad = 25
println("¡Hola, mi edad es $edad años!")
```

En este caso, la variable "edad" se inserta dentro de la cadena entre los símbolos "$" y es impresa en la consola como "¡Hola, mi edad es 25 años!".

Otra forma de concatenar cadenas es mediante el método "plus()".

```Kotlin
val frase1 = "Hola"
val frase2 = "mundo"
val frase3 = frase1.plus(" ").plus(frase2)
println(frase3)
```

En este ejemplo, los dos métodos "plus()" se utilizan para unir dos cadenas y se imprime en la consola como "Hola mundo".

## Profundizando

Es importante tener en cuenta que la concatenación de cadenas puede ser ineficiente si se realiza con una gran cantidad de cadenas o en un bucle. En esos casos, es mejor utilizar la clase "StringBuilder", que permite crear y modificar cadenas de forma más eficiente.

```Kotlin
val lista = listOf("Juan", "María", "Pedro")
val builder = StringBuilder()
for (nombre in lista) {
    builder.append(nombre).append(" ")
}
val nombres = builder.toString()
println(nombres)
```

En este ejemplo, se utiliza la clase "StringBuilder" para crear una cadena que contenga una lista de nombres, separados por un espacio.

## Ver también

- [Documentación de concatenación de cadenas en Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Ejemplos de cadenas en Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [Utilizar StringBuilder en Kotlin](https://www.geeksforgeeks.org/using-stringbuilder-class-kotlin/)