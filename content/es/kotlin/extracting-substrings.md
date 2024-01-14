---
title:    "Kotlin: Extrayendo subcadenas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una tarea común en la programación, ya sea para procesar datos o manipular cadenas de texto en aplicaciones. Este artículo explorará cómo extraer subcadenas en Kotlin y proporcionará ejemplos de código para ayudarte a entender mejor este concepto.

## Cómo hacerlo

En Kotlin, hay diferentes formas de extraer subcadenas, dependiendo de tus necesidades. Una de las formas más comunes es utilizando el método `substring()` de la clase `String`. Este método toma dos parámetros: el índice inicial y el índice final de la subcadena que deseas extraer. Veamos un ejemplo:

```Kotlin
val texto = "Hola mundo"
val subcadena = texto.substring(5, 10)
print(subcadena) //output: mundo
```

En este ejemplo, hemos creado una variable `texto` que contiene una cadena y luego hemos utilizado el método `substring()` para extraer la subcadena "mundo" comenzando desde el índice 5 hasta el 10.

Si no especificamos el índice final, el método `substring()` tomará todo el texto desde el índice inicial hasta el final. Por ejemplo:

```Kotlin
val subcadena = texto.substring(5)
print(subcadena) //output: mundo
```

Otra forma de extraer subcadenas es utilizando el operador de rango `..`. Este operador nos permite especificar un rango de índices en lugar de los valores exactos. Por ejemplo:

```Kotlin
val subcadena = texto[5..10]
print(subcadena) //output: mundo
```

En este caso, también obtenemos la subcadena "mundo" comenzando desde el índice 5 hasta el 10.

También podemos utilizar `substringAfter()` o `substringBefore()` para extraer una subcadena después o antes de un determinado carácter. Por ejemplo:

```Kotlin
val correo = "usuario@dominio.com"
val usuario = correo.substringBefore("@")
print(usuario) //output: usuario
```

## Inmersión profunda

Además de las formas básicas de extraer subcadenas que hemos visto, en Kotlin también podemos utilizar expresiones regulares para formatear y manipular cadenas de texto. Las expresiones regulares son patrones que nos permiten buscar y reemplazar partes específicas de una cadena.

Por ejemplo, si tenemos una cadena que contiene una dirección de correo electrónico, podemos utilizar una expresión regular para extraer el nombre de usuario antes del "@" y el dominio después del "@".

```Kotlin
val correo = "usuario@dominio.com"
val usuarioRegex = Regex("(.+)@(.+)")
val usuarioMatch = usuarioRegex.find(correo)
val usuario = usuarioMatch?.groupValues?.get(1)
val dominio = usuarioMatch?.groupValues?.get(2)
print("Usuario: $usuario, Dominio: $dominio") //output: Usuario: usuario, Dominio: dominio.com
```

En este caso, hemos utilizado una expresión regular para buscar un patrón en la cadena `correo`. Luego, utilizamos el método `find()` para obtener los resultados y el método `groupValues` para acceder a los valores de los grupos capturados por la expresión regular.

## Ver también

- [Documentación de Kotlin sobre String.substring()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Ejemplos de uso de expresiones regulares en Kotlin](https://www.baeldung.com/kotlin/regex-matchers)
- [Tutorial de Kotlin: Trabajando con cadenas de texto](https://www.vogella.com/tutorials/KotlinStrings/article.html)