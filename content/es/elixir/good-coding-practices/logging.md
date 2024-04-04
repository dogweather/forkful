---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-25 02:03:45.600637-07:00
description: "C\xF3mo hacerlo: En Elixir, la forma principal de registrar informaci\xF3\
  n es a trav\xE9s del m\xF3dulo incorporado `Logger`. As\xED es como puedes usarlo."
lastmod: '2024-04-04T00:26:52.259942-06:00'
model: gpt-4-0125-preview
summary: "En Elixir, la forma principal de registrar informaci\xF3n es a trav\xE9\
  s del m\xF3dulo incorporado `Logger`."
title: Registro
weight: 17
---

## Cómo hacerlo:
En Elixir, la forma principal de registrar información es a través del módulo incorporado `Logger`. Así es como puedes usarlo:

```elixir
defmodule MiAplicacion do
  require Logger

  def hacer_algo_importante(param) do
    Logger.info("Iniciando proceso importante con parámetro: #{param}")

    # Simular trabajo siendo realizado
    :timer.sleep(1000)

    Logger.debug("Proceso completado.")
  rescue
    error -> Logger.error("Ocurrió un error: #{inspect(error)}")
  end
end

# Para ver tus registros, solo llama a la función:
MiAplicacion.hacer_algo_importante("MiParametro")
```

Este sencillo fragmento muestra cómo registrar a diferentes niveles (`info`, `debug` y `error`). Cuando ejecutes esto, no verás el mensaje de depuración a menos que configures el nivel de Logger en `:debug`. Por defecto, el Logger de Elixir filtra los mensajes de registro por debajo de `:info`.

Un ejemplo de salida en el nivel de `:info` podría verse así:
```
14:32:40.123 [info]  Iniciando proceso importante con parámetro: MiParametro
14:32:41.126 [error] Ocurrió un error: %RuntimeError{message: "error de ejecución"}
```

## Análisis Profundo:
El `Logger` de Elixir es una herramienta incorporada que ha sido parte del lenguaje desde sus primeros días. Está influenciado por los sistemas de registro de otros lenguajes BEAM como Erlang. El registrador ofrece diferentes niveles de registro – `:debug`, `:info`, `:warn`, y `:error` – y es adaptable, lo que permite que diferentes backends se conecten para manejar los mensajes de registro.

Una alternativa al Logger incorporado para escenarios más complejos es el uso de bibliotecas de registro como `Logstash` o `Sentry` para Elixir, que pueden proporcionar características adicionales como seguimiento de errores y agregación en un formato más visual. Para el desarrollo local, los desarrolladores de Elixir a menudo confían en la funcionalidad del Logger incorporado por su simplicidad e integración con la VM BEAM.

Bajo el capó, el módulo Logger ofrece registro asincrónico y sincrónico. El registro asincrónico, que es el predeterminado, no bloquea la ejecución de tu aplicación mientras registra los mensajes. Esto asegura que el registro no afecte negativamente el rendimiento. Sin embargo, el registro sincrónico se puede habilitar para casos en los que necesite garantizar que los mensajes se registren en el orden en que se enviaron.

La configuración del Logger se puede ajustar en el archivo `config/config.exs` de una aplicación Elixir, donde puedes establecer el nivel de registro, formato, metadatos y más. Recuerda siempre ajustar tus niveles de registro y salidas para diferentes entornos; no querrías que registros detallados de depuración inundaran tus sistemas de producción.

## Ver También:
- La documentación oficial de Logger de Elixir: https://hexdocs.pm/logger/Logger.html
- Un post en el blog sobre las mejores prácticas de registro en Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry para Elixir en Hex: https://hex.pm/packages/sentry
- La lección sobre Logger de Elixir School: https://elixirschool.com/en/lessons/specifics/debugging/#logging
