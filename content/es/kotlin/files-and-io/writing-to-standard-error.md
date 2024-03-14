---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:41.942388-07:00
description: "Escribir en el error est\xE1ndar (stderr) se trata de enviar mensajes\
  \ de error y diagn\xF3sticos a un flujo separado, distinto del salida est\xE1ndar\
  \ (stdout), lo\u2026"
lastmod: '2024-03-13T22:44:59.052267-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) se trata de enviar mensajes de\
  \ error y diagn\xF3sticos a un flujo separado, distinto del salida est\xE1ndar (stdout),\
  \ lo\u2026"
title: "Escribiendo en el error est\xE1ndar"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Escribir en el error estándar (stderr) se trata de enviar mensajes de error y diagnósticos a un flujo separado, distinto del salida estándar (stdout), lo que permite un mejor manejo de errores y análisis de registros. Los programadores hacen esto para facilitar la depuración y asegurar que los mensajes de error se puedan identificar fácilmente y redirigir si es necesario, manteniendo registros de salida limpios o mensajes para el usuario.

## Cómo hacerlo:

En Kotlin, escribir en stderr se puede lograr utilizando `System.err.println()`. Este método es similar a `System.out.println()`, pero dirige la salida al flujo de error estándar en lugar del flujo de salida estándar.

```kotlin
fun main() {
    System.err.println("¡Este es un mensaje de error!")
}
```

Salida de muestra:
```
¡Este es un mensaje de error!
```

Para aplicaciones más estructuradas o complejas, particularmente aquellas que involucran marcos de registro como Logback o SLF4J, puedes configurar los registradores para escribir en stderr para ciertos niveles de registro (por ejemplo, ERROR).

Usando SLF4J con Logback:

1. Primero, agrega la API SLF4J y la implementación de Logback a tu `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. A continuación, configura Logback (en `src/main/resources/logback.xml`) para dirigir mensajes de nivel de error a stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Luego, utiliza SLF4J en tu código Kotlin para registrar mensajes de error:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("EjemploLogger")
    logger.error("¡Este es un mensaje de registro de error!")
}
```

Salida de muestra (a stderr):
```
2023-04-01 12:34:56 [main] ERROR EjemploLogger - ¡Este es un mensaje de registro de error!
```
