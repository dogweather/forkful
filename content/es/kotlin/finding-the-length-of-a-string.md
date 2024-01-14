---
title:    "Kotlin: Encontrando la longitud de una cadena"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué encontrar la longitud de una cadena

En la programación, a menudo nos encontramos trabajando con cadenas de texto. Ya sea para validar datos o para manipularlos de alguna manera, es importante entender cómo trabajar con ellos. Una de las operaciones más comunes que se realizan en cadenas es determinar su longitud. En este post, aprenderemos por qué es importante encontrar la longitud de una cadena y cómo hacerlo en Kotlin.

## Cómo hacerlo

La forma más sencilla de encontrar la longitud de una cadena en Kotlin es utilizando la propiedad `length`. Esta propiedad devuelve un valor entero que representa la cantidad de caracteres en la cadena. Veamos un ejemplo de cómo usarlo en un código:

```kotlin
val cadena = "Hola Mundo"
val longitud = cadena.length
println("La longitud de la cadena es: " + longitud) // output: La longitud de la cadena es: 10
```

También podemos utilizar métodos de extensión como `count()` o `size()` para encontrar la longitud de una cadena. Estos métodos funcionan de manera similar a `length` y nos permiten ser más dinámicos en nuestra forma de codificar. Veamos otro ejemplo:

```kotlin
val cadena = "Me gusta programar"
println(cadena.size) // output: 19
```

## Profundizando

Es importante tener en cuenta que, aunque `length` y otros métodos de extensión nos ayudan a encontrar la longitud de una cadena rápidamente, debemos tener en cuenta algunos detalles al trabajar con cadenas. Por ejemplo, en Kotlin, las cadenas se tratan como objetos inmutables, lo que significa que no se pueden cambiar una vez que se han creado. Si por alguna razón necesitamos modificar una cadena, deberemos crear una nueva. Esto puede afectar la cantidad de caracteres que se devuelven al utilizar la propiedad `length` o los métodos de extensión mencionados anteriormente.

También es importante mencionar que en Kotlin, los emojis y otros caracteres especiales pueden ocupar más de un carácter en una cadena. Esto afectará la longitud que se devuelve y debemos tenerlo en cuenta al manipular cadenas en nuestras aplicaciones.

## Ver también

- [Documentación oficial de Kotlin sobre cadenas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Cómo trabajar con cadenas en Kotlin](https://devexperto.com/cadenas-string-kotlin/)
- [Operaciones básicas con cadenas en Kotlin](https://medium.com/@mauvuelles/operaciones-b%C3%A1sicas-con-cadenas-en-kotlin-adbd6974845e)