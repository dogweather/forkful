---
title:                "Registro de Actividades en Programación"
date:                  2024-01-26T01:03:39.713333-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
El registro de actividad (logging) es esencialmente nuestra forma de registrar lo que ocurre en nuestros programas. Es como tener una pequeña caja negra; cuando las cosas van mal (y créeme, sucederá), los registros son invaluables para descubrir qué pasó, diagnosticar problemas y optimizar el rendimiento.

## Cómo hacerlo:
En Gleam, normalmente incorporarías una biblioteca de registro—no hay un mecanismo de registro dedicado listo para usar. Digamos que estamos usando una hipotética crate `gleam_logger`. Así es como podrías integrarla:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("¡La aplicación está iniciando!")
  let result = calculo_intenso()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Cálculo exitoso", value)
    Error(err) -> 
      gleam_logger.error("Cálculo fallido", err)
  }
}
```

La salida esperada en tus registros se vería algo así:

```
INFO: ¡La aplicación está iniciando!
DEBUG: Cálculo exitoso 42
ERROR: Cálculo fallido Motivo: División por cero
```

## Profundización
El arte de registrar actividades ha existido desde los primeros días de la programación. Los operadores de sistemas literalmente obtenían registros de la computadora - asegurándose de que todo funcionara sin problemas. Avanzamos rápidamente y el registro de actividades se ha digitalizado, convirtiéndose en una parte central del desarrollo de software.

Aunque Gleam, siendo un lenguaje relativamente joven que apunta al ecosistema de Erlang, no tiene un marco de registro integrado, puedes aprovechar las instalaciones de registro de Erlang maduras o otras bibliotecas proporcionadas por la comunidad. Cada uno tiene diferentes características y compromisos: algunos pueden proporcionar registro estructurado, otros están más para salida de texto sencillo.

Ahora, la cuestión de implementar una instalación de registro: ¿Es sencillo? A primera vista, sí. Pero si profundizas, estás mirando el manejo de concurrencia, cuellos de botella de E/S, rotación de registros, estandarización de formatos (piensa en JSON para registro estructurado), filtrado de niveles y, posiblemente, rastreo distribuido. Además, en un paradigma funcional, generalmente quieres que los efectos secundarios (como el registro) se manejen de manera predecible y controlada.

## Ver También
Aquí es donde puedes encontrar más sobre los entresijos del registro de actividad en Gleam y su ecosistema circundante:
- [Documentación de :logger de Erlang](http://erlang.org/doc/apps/kernel/logger_chapter.html): Dado que Gleam compila a Erlang, esto es directamente aplicable.
- [Documentos de la biblioteca estándar de Gleam](https://hexdocs.pm/gleam_stdlib/): Para actualizaciones sobre cualquier utilidad de registro que pueda añadirse.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Una lista curada de recursos, que podría incluir bibliotecas de registro a medida que estén disponibles.