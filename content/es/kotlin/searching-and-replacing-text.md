---
title:                "Buscando y reemplazando texto"
html_title:           "Kotlin: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en Kotlin?

A veces, en la programación, necesitamos realizar cambios en textos largos o en múltiples archivos. En lugar de hacerlo manualmente, podemos utilizar la funcionalidad de búsqueda y reemplazo en Kotlin para ahorrar tiempo y esfuerzo.

## Cómo buscar y reemplazar texto en Kotlin

Para realizar una búsqueda y reemplazo en Kotlin, podemos utilizar la función `replace` en un `String`. Aquí hay un ejemplo:

```Kotlin
val texto = "¡Hola a todos! Este es un ejemplo de texto."
val nuevoTexto = texto.replace("todos", "amigos")
println(nuevoTexto)
```

La salida de este código sería "*¡Hola a amigos! Este es un ejemplo de texto.*". Como podemos ver, la palabra "todos" ha sido reemplazada por "amigos". También podemos utilizar expresiones regulares en la función `replace` para realizar cambios más complejos.

## Profundizando en la búsqueda y reemplazo en Kotlin

La función `replace` también acepta un parámetro llamado `regex`, que nos permite utilizar expresiones regulares para hacer reemplazos más avanzados. Por ejemplo, si queremos reemplazar todas las letras mayúsculas en un texto con asteriscos, podemos hacerlo de la siguiente manera:

```Kotlin
val texto = "Este es un TEXTO de ejemplo."
val nuevoTexto = texto.replace(Regex("[A-Z]"), "*")
println(nuevoTexto)
```

La salida sería "*ste es un *EXTO de ejemplo.*". Como podemos ver, todas las letras mayúsculas han sido reemplazadas por asteriscos. Esto es especialmente útil cuando queremos realizar cambios en un texto que sigue cierto patrón.

## Ver también

- [Documentación oficial de Kotlin sobre búsqueda y reemplazo en Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Expresiones regulares en Kotlin](https://kotlinlang.org/docs/regular-expressions.html)
- [Tutorial de Kotlin para principiantes](https://www.javatpoint.com/kotlin-tutorial)