---
title:                "Registro de Actividades"
date:                  2024-01-26T01:08:19.601292-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La creación de registros (logging) en programación es como mantener un diario para tu aplicación. Es el registro sistemático de eventos, mensajes y puntos de datos que te ofrecen una visión de lo que tu aplicación está haciendo y cómo se está comportando. Los programadores registran porque es crucial para depurar, monitorear la salud de la aplicación y obtener pistas sobre problemas potenciales antes de que se conviertan en problemas reales.

## Cómo hacerlo:
Ruby viene con un módulo integrado para la creación de registros, `Logger`, que es super fácil de usar. Aquí tienes un ejemplo rápido para comenzar:

```ruby
require 'logger'

# Crea un Logger que saca la salida a STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Ejemplos de mensajes de registro
logger.info("Este es un mensaje informativo")
logger.warn("Este es un mensaje de advertencia")
logger.error("Este es un mensaje de error")
```

Ejecutar el script anterior producirá algo como esto:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Este es un mensaje informativo
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Este es un mensaje de advertencia
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Este es un mensaje de error
```

Puedes configurar el formato de registro y el nivel para filtrar el ruido innecesario, y puedes dirigir los registros a diferentes salidas, como un archivo o incluso un servicio de registro externo.

## Más a fondo
La creación de registros es como una antigua tradición en programación. Históricamente, los registros eran simples archivos de texto, analizados manualmente con herramientas como `grep`. Pero el concepto se convirtió en todo un ecosistema de robustos marcos de trabajo y servicios de registro como Log4j, Syslog en Linux, o Sematext y Loggly en la era de la nube.

El `Logger` de Ruby es una forma sencilla de empezar, pero si necesitas más potencia y flexibilidad, podrías echar un vistazo a alternativas como Lograge o Semantic Logger. Estas bibliotecas se integran bien con las aplicaciones de Ruby, ofreciendo un control más granular sobre el formato de registro, incluyendo registros estructurados (formato JSON), mejor rendimiento y una integración perfecta con otros servicios.

Cada biblioteca de registro de Ruby tiene su propia forma de hacer las cosas, pero en esencia, todas giran en torno a la idea de una instancia de registrador a la que envías mensajes. El registrador maneja estos mensajes basándose en niveles establecidos—DEBUG, INFO, WARN, ERROR, FATAL y UNKNOWN—y decide qué hacer con ellos: imprimirlos, guardarlos en un archivo, enviarlos a través de la red, etc.

## Ver También
Para un estudio más profundo del módulo de registro integrado de Ruby, consulta la documentación oficial:

Si estás interesado en registros más avanzados o quieres explorar gemas de terceros:
- [Lograge](https://github.com/roidrage/lograge)

Para prácticas y filosofía de registro en general (no específicas de Ruby), estos artículos son lecturas eternas:
- [Libro de Ingeniería de Fiabilidad del Sitio de Google - Capítulo 16: Manejo de Sobrecarga](https://sre.google/sre-book/handling-overload/#log-messages)
- [La aplicación de 12 factores - Registros](https://12factor.net/logs)
