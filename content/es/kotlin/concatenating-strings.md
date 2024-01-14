---
title:                "Kotlin: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica común en la programación para unir dos o más cadenas de texto en una sola. Esto es útil cuando se desea combinar palabras, oraciones o incluso variables en una sola cadena. Puede ser especialmente útil en aplicaciones web o móviles donde se necesita mostrar información al usuario.

## Cómo hacerlo

Para concatenar cadenas en Kotlin, se puede utilizar el operador `+` o el método `plus()`. Veamos un ejemplo:

```Kotlin
val nombre = "Juan"
val apellido = "García"
val nombreCompleto = nombre + " " + apellido // "Juan García"
val edad = 25
val info = "Me llamo " + nombreCompleto + " y tengo " + edad + " años." // "Me llamo Juan García y tengo 25 años."
```

También se puede usar el método `format()` para incluir variables en una cadena de formato:

```Kotlin
val nombre = "Sofía"
val apellido = "López"
val nombreCompleto = "%s %s".format(nombre, apellido) // "Sofía López"
val edad = 30
val info = "Me llamo %s y tengo %d años.".format(nombreCompleto, edad) // "Me llamo Sofía López y tengo 30 años."
```

Como se puede ver en los ejemplos, se pueden concatenar cadenas de texto con otros tipos de datos, como enteros.

## Deep Dive

En Kotlin, las cadenas son inmutables, lo que significa que no se pueden modificar una vez creadas. Entonces, al concatenar cadenas, en realidad se están creando nuevas cadenas en lugar de modificar las existentes. Esto puede no ser eficiente si se están manejando grandes cadenas de texto, ya que se estarían creando varias copias. En estos casos, se recomienda utilizar la clase `StringBuilder`, que permite modificar una cadena sin crear copias adicionales.

También es importante tener en cuenta que la concatenación repetida de cadenas puede ralentizar el rendimiento de una aplicación, ya que requiere una gran cantidad de asignaciones y creación de objetos. Por lo tanto, es una buena práctica usar la clase `StringBuilder` o el método `format()` en lugar de una concatenación tradicional.

## Ver también
- [Documentación de Kotlin sobre la clase StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- [Más información sobre concatenación de cadenas en Kotlin](https://www.baeldung.com/kotlin/concat-strings)