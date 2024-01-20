---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Encontrar la longitud de una cadena (string) significa determinar cuántos caracteres contiene esa cadena. Los programadores hacen esto para controlar la entrada de datos, procesar texto y muchas otras razones.

## ¿Cómo hacerlo?
Usar la propiedad `length` en Kotlin es bastante sencillo. Mira el siguiente código:

```Kotlin
fun main() {
    val cadena = "¡Hola, mundo!"
    println("El número de caracteres en '$cadena' es: ${cadena.length}")
}
```

Este código imprimirá: 
```
El número de caracteres en '¡Hola, mundo!' es: 13
```
Esto es porque la cadena "¡Hola, mundo!" tiene 13 caracteres, incluyendo espacios y signos de puntuación.

## Inmersión profunda
- **Contexto histórico**: Desde los albores de la informática, la necesidad de conocer la longitud de una cadena siempre ha sido crucial.
  
- **Alternativas**: Aunque la mayoría de lenguajes, incluido Kotlin, usan alguna variante de `length` para obtener la longitud de una cadena, otros usan funciones específicas, como `strlen` en C.

- **Detalles de implementación**: El método `length` en Kotlin funciona bajo el capó utilizando las funciones de nivel más bajo del sistema operativo para contar los caracteres.

## Ver también
Echa un vistazo a estos recursos para obtener más información (*Links simulados*):
- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/strings.html#string-length)
- [Tutorial sobre Strings en Kotlin](http://www.myexample.com/kotlin-string)
- [Trabajar con Strings en Kotlin](https://www.fakeexample.com/kotlin-string-tutorial)