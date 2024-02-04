---
title:                "Escribiendo en el error estándar"
date:                  2024-02-03T19:33:30.444282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir en el error estándar (stderr) implica enviar mensajes de error y diagnósticos a la consola o terminal. Los programadores lo hacen para separar la información de error de la salida estándar (stdout), facilitando la depuración y el análisis de logs.

## Cómo hacerlo:

### Salida básica a stderr en Java
Java proporciona una forma sencilla de escribir en stderr utilizando `System.err.print()` o `System.err.println()`. Así es cómo se hace:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Error: No se puede dividir por cero.");
        }
    }
}
```

Salida de ejemplo:

```
Error: No se puede dividir por cero.
```

Esto imprimirá directamente el mensaje de error al flujo de error estándar.

### Uso de un Logger para el Manejo Avanzado de Errores
Para aplicaciones que necesitan un manejo de errores y registro más sofisticados, es común usar una biblioteca de registro como SLF4J con Logback o Log4J2. Esto permite más flexibilidad en la gestión de la salida de errores, incluyendo la redirección de archivos, filtrado y formateo.

#### Ejemplo con Logback

Primero, añade la dependencia de Logback a tu archivo `pom.xml` (Maven) o `build.gradle` (Gradle). Para Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Luego, puedes usar el siguiente código para registrar errores:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Error: No se puede dividir por cero.", e);
        }
    }
}
```

Esto imprimirá el mensaje de error junto con un seguimiento de la pila en la consola o un archivo, dependiendo de la configuración de Logback.

Usar marcos de trabajo para registro como Logback proporciona más control sobre el manejo de errores, facilitando la gestión de aplicaciones y sistemas grandes.
