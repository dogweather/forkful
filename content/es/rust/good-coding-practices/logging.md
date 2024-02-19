---
aliases:
- /es/rust/logging/
date: 2024-01-26 01:08:09.374496-07:00
description: "Hacer logging es como mantener un diario para tu aplicaci\xF3n; es la\
  \ pr\xE1ctica de registrar eventos, errores y otros datos pertinentes durante la\
  \ ejecuci\xF3n.\u2026"
lastmod: 2024-02-18 23:09:09.756077
model: gpt-4-1106-preview
summary: "Hacer logging es como mantener un diario para tu aplicaci\xF3n; es la pr\xE1\
  ctica de registrar eventos, errores y otros datos pertinentes durante la ejecuci\xF3\
  n.\u2026"
title: Registro de Actividades
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Hacer logging es como mantener un diario para tu aplicación; es la práctica de registrar eventos, errores y otros datos pertinentes durante la ejecución. Los desarrolladores usan los logs para diagnosticar problemas, monitorear el comportamiento del sistema y recopilar información que impulsa mejoras—es el pan de cada día de la inteligencia operacional.

## Cómo hacerlo:

Configuramos un escenario básico de logging en Rust usando el crate `log`, que proporciona una fachada de logging, y `env_logger`, una implementación de logging para el crate `log`. Primero, agrégalos a tu Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Ahora, configura e inicializa el logger en tu `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Este es un mensaje informativo.");
    warn!("Este es un mensaje de advertencia.");
}
```

Ejecuta tu aplicación con `RUST_LOG=info cargo run`, y verás la salida:

```
INFO: Este es un mensaje informativo.
WARN: Este es un mensaje de advertencia.
```

Experimenta con la variable de entorno `RUST_LOG` estableciéndola en `error`, `warn`, `info`, `debug` o `trace` para controlar la verbosidad de tus logs.

## Análisis Profundo

El concepto de hacer logging no es nada nuevo; ha existido desde los primeros días de la computación. Antes de que el logging fuera común en el software, los desarrolladores dependían de métodos primitivos como declaraciones de impresión o herramientas de depuración para rastrear la ejecución del programa. A medida que los programas aumentaban en complejidad, también lo hacía la necesidad de enfoques estructurados para el logging.

En Rust, el crate `log` abstrae los detalles de la implementación del logging, permitiendo a los desarrolladores enchufar diferentes backends de logging. Mientras que `env_logger` es una opción común, hay alternativas como `fern`, `slog` o `tracing`, cada una con su propio conjunto de características y opciones de configuración.

Algunas consideraciones al implementar logging incluyen:

1. **Niveles de Log**: Controlar la verbosidad es clave. El crate `log` de Rust define varios niveles de log: error, warn, info, debug y trace, en orden decreciente de severidad.

2. **Rendimiento**: El logging puede afectar el rendimiento. Es crítico usarlo con juicio, asegurándose de evitar el logging en caminos críticos para el rendimiento o logs excesivamente verbosos en producción.

3. **Logging Estructurado**: Las mejores prácticas modernas involucran un logging estructurado, donde los logs se escriben en un formato legible por máquinas como JSON. Librerías como `slog` permiten el logging estructurado en Rust, que pueden ser indexados y consultados usando sistemas de gestión de logs como ELK Stack o Splunk.

4. **Logging Asíncrono**: Para minimizar el impacto en la aplicación principal, el logging se puede realizar de manera asincrónica. Esto a menudo se logra haciendo que la librería de logging escriba en una cola en memoria, y un hilo separado procesa la cola y escribe los logs al destino.

5. **Configuración**: Muchos frameworks de logging admiten configuración a través de variables de entorno, archivos de configuración y/o código. Esta flexibilidad es clave para afinar la salida en diferentes entornos (desarrollo, staging, producción).

## Ver También

- Documentación del crate `log`: https://docs.rs/log/
- Documentación del crate `env_logger`: https://docs.rs/env_logger/
- Página de logging de Rust by Example: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- El crate `slog`, un framework de logging alternativo: https://github.com/slog-rs/slog
- Tracing, un framework para instrumentar programas Rust: https://crates.io/crates/tracing
