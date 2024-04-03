---
date: 2024-01-26 01:06:48.391297-07:00
description: "El registro (logging) es esencialmente el proceso de grabar eventos\
  \ que ocurren dentro de una aplicaci\xF3n de software. Los programadores registran\
  \ estos\u2026"
lastmod: '2024-03-13T22:44:58.943793-06:00'
model: gpt-4-1106-preview
summary: "El registro (logging) es esencialmente el proceso de grabar eventos que\
  \ ocurren dentro de una aplicaci\xF3n de software."
title: Registro de Actividades
weight: 17
---

## ¿Qué y Por Qué?
El registro (logging) es esencialmente el proceso de grabar eventos que ocurren dentro de una aplicación de software. Los programadores registran estos eventos para capturar información en tiempo de ejecución, depurar problemas, monitorear el comportamiento del sistema y crear un registro de auditoría para fines de seguridad y cumplimiento.

## Cómo hacerlo:
Aquí hay una manera simple de comenzar con el registro en Java utilizando el paquete integrado `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class RegistroDeAplicacion {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Registrando un mensaje a nivel INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Ocurrió una excepción", e);
        }
    }
}
```

Esto produciría una salida similar a:

```
Jul 03, 2023 2:00:00 PM RegistroDeAplicacion main
INFO: Registrando un mensaje a nivel INFO
Jul 03, 2023 2:00:00 PM RegistroDeAplicacion main
SEVERE: Ocurrió una excepción
java.lang.ArithmeticException: / por cero
    at RegistroDeAplicacion.main(RegistroDeAplicacion.java:10)
```

## Estudio Profundo
El registro en Java ha evolucionado bastante. Históricamente, el registro era más ad-hoc con salidas del sistema y mecanismos escritos por uno mismo. Sin embargo, la necesidad de estandarización llevó a APIs de registro como `Log4j` y `SLF4J`. El paquete `java.util.logging` fue introducido en JDK 1.4, proporcionando una manera estandarizada de registrar mensajes.

Las alternativas a `java.util.logging` (JUL) incluyen Log4j 2 y SLF4J. Mientras que JUL viene incorporado en Java y por lo tanto no requiere dependencias adicionales, tanto Log4j 2 como SLF4J ofrecen características más avanzadas como un control más granular sobre la configuración del registro, registro asincrónico y mejor rendimiento.

En cuanto a la implementación, el registro puede ser ya sea sincrónico, donde cada mensaje de registro es procesado en el hilo que lo generó, o asincrónico, donde los mensajes se pasan a un hilo separado. El registro asincrónico puede mejorar el rendimiento pero introduce complejidad ya que se debe manejar la concurrencia y asegurar que los mensajes de registro no se pierdan en caso de una caída de la aplicación.

## Vea También
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Resumen oficial de registro de Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial sobre java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
