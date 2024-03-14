---
date: 2024-01-26 01:00:56.433147-07:00
description: "El registro de eventos (logging) es esencialmente el equivalente en\
  \ software de un diario de navegaci\xF3n; es una forma de registrar eventos que\
  \ ocurren\u2026"
lastmod: '2024-03-13T22:44:58.663249-06:00'
model: gpt-4-1106-preview
summary: "El registro de eventos (logging) es esencialmente el equivalente en software\
  \ de un diario de navegaci\xF3n; es una forma de registrar eventos que ocurren\u2026"
title: "Registro de Actividades en Programaci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El registro de eventos (logging) es esencialmente el equivalente en software de un diario de navegación; es una forma de registrar eventos que ocurren mientras una aplicación se está ejecutando. Los programadores lo hacen para mantener un seguimiento de estos eventos para la depuración, auditoría o para obtener información sobre el comportamiento de un sistema en producción.

## Cómo hacerlo:
Clojure se apoya en las facilidades de registro de Java, pero puedes acceder a ellas de una manera más idiomática de Clojure. Veamos cómo podrías usar `clojure.tools.logging`, que proporciona una abstracción simple sobre varios frameworks de registro:

Primero, añade una dependencia para `clojure.tools.logging` y una implementación de registro como `log4j` en tu `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Ahora, vamos a registrar algunos mensajes:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Iniciando computación intensa...")
  (Thread/sleep 3000) ; Simulando una larga computación
  (log/info "Computación terminada. La respuesta es 42.")
  42)

(compute-answer-to-everything)
```
La salida no mostrará los mensajes `DEBUG` por defecto, ya que los niveles de registro se configuran típicamente en `INFO`:

```
INFO  [tu-namespace] - Computación terminada. La respuesta es 42.
```

Puedes configurar los niveles de registro y los appenders en un archivo `log4j.properties` para obtener una salida más detallada si es necesario.

## Profundización
`clojure.tools.logging` de Clojure ha existido por un tiempo y sirve como un puente entre el código de Clojure y el mundo de registro de Java. Históricamente, Java ha pasado por varias iteraciones y bibliotecas para registros como la API de registro incorporada de Java, `log4j`, `slf4j` y `logback`.

En Clojure, aunque puedes usar directamente los frameworks de registro de Java, `clojure.tools.logging` detecta y delega al framework de registro que encuentre en tu classpath, evitando que estés fuertemente acoplado a una implementación específica. Esto puede ayudar a mantener tu código Clojure más portátil y modular.

Alternativas a `clojure.tools.logging` dentro del ecosistema de Clojure incluyen bibliotecas como `timbre`, que es una biblioteca de registro puramente Clojure con características como rotación de registros, filtrado y registro asíncrono de serie.

Los detalles de implementación son cruciales cuando se trata de registro en un entorno multi-hilo como Clojure. Aquí, la inmutabilidad y la gestión de efectos secundarios ofrecen ventajas distintas. El registro, como un efecto secundario, debería manejarse con cuidado para evitar cuellos de botella en el rendimiento y asegurar la seguridad entre hilos, lo cual la mayoría de los frameworks de registro de Java ya se encargan de hacer.

Por último, considera el registro estructurado, donde los registros se escriben como datos estructurados (como JSON). Esto puede ser extremadamente útil para análisis y procesamiento posteriores, especialmente cuando se trata de sistemas distribuidos a gran escala.

## Ver también
Si tienes ganas de más, considera consultar estos recursos:

- Documentación de Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, una biblioteca de registro de Clojure: https://github.com/ptaoussanis/timbre
- Configurando Log4J en Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Manual de Logback para configuraciones avanzadas: http://logback.qos.ch/manual/
- Una guía sobre registro estructurado en Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
