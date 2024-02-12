---
title:                "Escribiendo en el error estándar"
aliases:
- /es/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:32:54.427919-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en el error estándar (stderr) en Elixir es un método para dirigir mensajes de error y diagnósticos por separado de la salida principal (stdout). Los programadores usan stderr para depurar y manejar errores sin llenar de basura la salida principal del programa, facilitando la identificación y resolución de problemas.

## Cómo hacerlo:

En Elixir, puedes usar funciones del módulo `IO` como `IO.puts/2` e `IO.warn/2` para escribir mensajes en el error estándar:

```elixir
# Escribiendo un mensaje simple en stderr
IO.puts(:stderr, "Error: ¡Algo salió mal!")

# Usando IO.warn, que es más semántico para advertencias/errores
IO.warn("Advertencia: ¡Estás a punto de exceder el límite!")
```

Salida de muestra en el terminal para `IO.puts/2`:
```
Error: ¡Algo salió mal!
```

Para `IO.warn/2`, la salida sería similar, pero `IO.warn/2` está específicamente diseñado para advertencias y podría incluir formateo o comportamiento adicional en futuras versiones de Elixir.

**Usando Bibliotecas de Terceros**

Aunque la biblioteca estándar de Elixir suele ser suficiente para manejar la salida del error estándar, podrías encontrar útiles bibliotecas como `Logger` para aplicaciones más complejas o para configurar diferentes niveles de registro y salidas.

Ejemplo usando `Logger` para sacar un mensaje de error:

```elixir
require Logger

# Configurar Logger para salida a stderr
Logger.configure_backend(:console, device: :stderr)

# Escribiendo un mensaje de error
Logger.error("Error: No se pudo conectar a la base de datos.")
```

Esta configuración dirige específicamente la salida de `Logger` a stderr, lo cual es útil para separar la registro de errores de los mensajes de registro estándar.
