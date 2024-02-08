---
title:                "Iniciando un nuevo proyecto"
aliases:
- es/kotlin/starting-a-new-project.md
date:                  2024-01-20T18:03:58.928830-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando un nuevo proyecto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Iniciar un nuevo proyecto es como poner la primera piedra de tu construcción digital; es el punto de partida para cualquier aplicación que planees desarrollar. Los programadores inician proyectos para transformar ideas en código ejecutable creando la estructura base que define la dirección del desarrollo.

## Cómo hacerlo:

Para empezar un proyecto de Kotlin usando IntelliJ IDEA, sigue estos pasos simples:

1. Abre IntelliJ IDEA y selecciona "New Project".
2. Elige Kotlin en el menú lateral y configura el JDK.
3. Elige un proyecto como "JVM | IDEA", si planeas trabajar con una aplicación de escritorio.
4. Ponle nombre a tu proyecto y selecciona una ubicación para el mismo. 
5. Haz click en "Finish".

Aquí tienes un "Hola Mundo" en Kotlin:

```kotlin
fun main(args: Array<String>) {
    println("¡Hola Mundo!")
}
```

Si ejecutas este código, obtendrás la siguiente salida:

```
¡Hola Mundo!
```

## Inmersión profunda:

Kotlin fue creado por JetBrains en 2011 y se ha convertido en uno de los lenguajes favoritos para desarrollar en Android, entre otros entornos. Antes de Kotlin, Java era el rey para desarrollos en Android, pero Kotlin llegó con sintaxis más concisa, seguridad contra nulos y interoperabilidad total con Java.

Alternativas a IntelliJ IDEA incluyen Eclipse y Android Studio; cada uno con soporte para Kotlin pero con distintos enfoques: Eclipse está más orientado a Java y Android Studio está optimizado para desarrollo Android.

Detalles importantes al iniciar un proyecto son la configuración del SDK, la elección de dependencias apropiadas y la estructura del proyecto. Acertar desde el inicio facilita el mantenimiento y la expansión del código.

## Ver También:

- Documentación oficial de Kotlin: [kotlinlang.org](https://kotlinlang.org/docs/reference/)
- Android Studio, optimizado para Kotlin en Android: [developer.android.com/studio](https://developer.android.com/studio)
- Tutorial interactivo de Kotlin: [play.kotlinlang.org](https://play.kotlinlang.org)
