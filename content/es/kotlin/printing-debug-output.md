---
title:    "Kotlin: Imprimiendo salida de depuración"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

##¿Por qué imprimir la salida de depuración en Kotlin?

Si eres un programador principiante o incluso si tienes experiencia en Kotlin, es común encontrarse con errores en tu código. A veces, estos errores pueden ser difíciles de encontrar y solucionar. Es por eso que imprimir la salida de depuración es una técnica útil para identificar y solucionar errores en tu código de una manera eficiente.

##¿Cómo hacerlo?

Imprimir la salida de depuración en Kotlin es bastante sencillo. Puedes utilizar la función "println()" para imprimir en la consola cualquier valor o variable que quieras verificar. Por ejemplo:

```Kotlin
var edad = 26
println("Mi edad es: " + edad)
```

El resultado en la consola sería:

```
Mi edad es: 26
```

También puedes imprimir mensajes de depuración con formatos específicos utilizando la función "printf()" de la clase "System.out". Por ejemplo:

```Kotlin
var nombre = "Juan"
var apellido = "Pérez"
System.out.printf("Mi nombre es %s y mi apellido es %s", nombre, apellido)
```

El resultado en la consola sería:

```
Mi nombre es Juan y mi apellido es Pérez
```

##Profundizando

Además de imprimir valores y variables, también puedes imprimir mensajes de depuración en diferentes puntos de tu código para seguir su ejecución y ver si se están cumpliendo las condiciones y operaciones que esperas. Esto puede ser muy útil para comprender cómo funciona tu código y detectar posibles errores.

Otra técnica útil es utilizar la función "Log" de la biblioteca de Android para imprimir mensajes de depuración en dispositivos móviles. Esto es especialmente útil cuando estás desarrollando una aplicación para Android utilizando Kotlin.

Recuerda que es importante no dejar mensajes de depuración en tu código una vez que hayas solucionado los errores, ya que pueden afectar el rendimiento de tu aplicación.

##Ver también

- [Documentación de Kotlin sobre salida de depuración](https://kotlinlang.org/docs/reference/control-flow.html#println-for-debugging)
- [Tutorial de Kotlin para principiantes](https://developer.android.com/kotlin?hl=es)
- [Preguntas frecuentes sobre Kotlin](https://kotlinlang.org/docs/reference/migrating-from-java.html#what-is-the-difference-between-kotlin-and-java)