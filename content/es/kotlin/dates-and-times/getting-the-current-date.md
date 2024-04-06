---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:06.420064-07:00
description: "C\xF3mo hacerlo: Kotlin no tiene su propia API de fecha y hora, sino\
  \ que depende de la Java Standard Library para esta funcionalidad. As\xED es como\
  \ puedes\u2026"
lastmod: '2024-03-13T22:44:59.046578-06:00'
model: gpt-4-0125-preview
summary: Kotlin no tiene su propia API de fecha y hora, sino que depende de la Java
  Standard Library para esta funcionalidad.
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:


### Usando Kotlin Estándar
Kotlin no tiene su propia API de fecha y hora, sino que depende de la Java Standard Library para esta funcionalidad. Así es como puedes obtener la fecha actual:

```kotlin
import java.time.LocalDate

fun main() {
    val hoy = LocalDate.now()
    println("Fecha de Hoy: $hoy")
}
```

**Salida de muestra:**
```
Fecha de Hoy: 2023-04-05
```

### Usando java.util.Date
Para operaciones que requieren tanto la fecha como la hora, podrías preferir `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val fechaActual = Date()
    println("Fecha y Hora Actual: $fechaActual")
}
```

**Salida de muestra:**
```
Fecha y Hora Actual: Mié Abr 05 15:20:45 GMT 2023
```

### Usando la Biblioteca Joda-Time
Antes de que Java 8 introdujera una nueva API de Fecha y Hora, Joda-Time era el estándar de-facto para operaciones de fecha-hora en Java y Kotlin. Aunque ya no es necesario para muchos proyectos, algunos todavía pueden usarlo por razones de legado o preferencia personal.

Agrega la biblioteca Joda-Time al archivo build.gradle de tu proyecto:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val hoy = LocalDate.now()
    println("Fecha de Hoy: $hoy")
}
```

**Salida de muestra:**
```
Fecha de Hoy: 2023-04-05
```

### Usando ThreeTenABP para Android
Para el desarrollo de Android, se recomienda utilizar el backport de la Java Time API a través del Proyecto de Backport Android ThreeTen para versiones anteriores a Android API Level 26.

Agrega la dependencia al archivo build.gradle de tu aplicación:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Inicialízalo en tu clase Application:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MiApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Luego, puedes usarlo así:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val hoy = LocalDate.now()
    println("Fecha de Hoy: $hoy")
}
```

**Salida de muestra:**
```
Fecha de Hoy: 2023-04-05
```
