---
title:    "Kotlin: Concatenando cadenas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una habilidad fundamental en la programación Kotlin. Esto permite combinar diferentes cadenas de texto para formar una sola, lo que puede ser útil en situaciones como imprimir mensajes personalizados o crear URLs dinámicas. En este artículo, te mostraremos cómo concatenar strings en Kotlin de manera sencilla y eficiente.

## Cómo hacerlo

La concatenación de strings en Kotlin se puede lograr de varias maneras. Una de ellas es utilizando el operador "+" para unir dos o más strings.

```Kotlin
val nombre = "Juan"
val saludo = "¡Hola, "
val mensaje = saludo + nombre + "!" // mensaje = "¡Hola, Juan!"
```

También se puede utilizar la función `plus()` para lograr el mismo resultado.

```Kotlin
val apellido = "González"
val nombreCompleto = saludo.plus(nombre).plus(" ").plus(apellido) // nombreCompleto = "¡Hola, Juan González!"

```

Otra opción es utilizar la función `format()` que permite insertar valores en una String utilizando placeholders (%s para strings, %d para enteros, %f para decimales, etc.).

```Kotlin
val edad = 25
val mensajePersonalizado = "Tengo %d años".format(edad) // mensajePersonalizado = "Tengo 25 años"
```

También es posible utilizar la interpolación de strings, que permite insertar valores directamente en una string mediante el uso de `$`.

```Kotlin
val lugar = "el parque"
val actividad = "correr"
val frase = "Me encanta $actividad en $lugar" // frase = "Me encanta correr en el parque"
```

## Profundizando

En Kotlin, cada vez que concatenamos strings, se crea un nuevo string en memoria. Esto puede ser ineficiente si se concatenan strings en un loop o en situaciones similares. Por eso, es recomendable utilizar la clase `StringBuilder` cuando se necesite concatenar varias veces una string.

```Kotlin
val opciones = listOf("verde", "rojo", "azul")
val sb = StringBuilder()

for (opcion in opciones) {
    sb.append(opcion).append(" ") // sb = "verde rojo azul "
}

val mensajeFinal = sb.toString() // mensajeFinal = "verde rojo azul "

```

## Ver también

Para más información sobre strings y funciones útiles en Kotlin, puedes consultar los siguientes enlaces:

- [Documentación oficial de Kotlin sobre Strings] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string-builder/)
- [Cómo concatenar strings en Java] (https://www.baeldung.com/java-string-concatenation)
- [Otros operadores útiles en Kotlin] (https://kotlinlang.org/docs/reference/basic-types.html#operations)