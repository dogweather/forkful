---
title:    "Elixir: Imprimiendo salida de depuración"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

La impresión de salida de depuración es una herramienta importante para todo programador de Elixir. Te permite tener una mejor visibilidad de lo que está sucediendo en tu código y te ayuda a identificar y solucionar errores de manera más eficiente.

## Cómo

La impresión de salida de depuración se puede realizar fácilmente utilizando la función `IO.inspect/2`. Esta función toma dos argumentos: el valor que deseas inspeccionar y una lista de opciones de impresión. Veamos un ejemplo:

```elixir
defmodule Persona do
  defstruct nombre: "Juan", apellido: "García", edad: 25
end

persona = %Persona{nombre: "María", apellido: "Pérez", edad: 30}
IO.inspect(persona, label: "Persona")
# Output: Persona: %Persona{nombre: "María", apellido: "Pérez", edad: 30}
```

En el ejemplo anterior, creamos una estructura llamada `Persona` y luego creamos una instancia de esa estructura asignándola a la variable `persona`. Luego, utilizamos `IO.inspect/2` para imprimir la información de la variable `persona` con una etiqueta de "Persona". Esto ayuda a identificar fácilmente qué valor estamos inspeccionando.

También podemos utilizar la opción `:pretty` para obtener una salida más legible:

```elixir
IO.inspect(persona, label: "Persona", pretty: true)
# Output:
# Persona:
# %Persona{
#   apellido: "Pérez",
#   edad: 30,
#   nombre: "María"
# }
```

Además, `IO.inspect/2` también es útil para inspeccionar valores en medio de una pipelina:

```elixir
persona
|> Map.update(:edad, &(&1 * 2))
|> IO.inspect(label: "Persona con edad duplicada")
# Output: Persona con edad duplicada: %{apellido: "Pérez", edad: 60, nombre: "María"}
```

## Buceo profundo

Además de la función `IO.inspect/2`, también podemos utilizar el módulo `Logger` para imprimir salidas de depuración. `Logger` proporciona diferentes niveles de registro (debug, info, warn, error) que se pueden utilizar para imprimir información dependiendo de su gravedad.

Veamos un ejemplo utilizando `Logger`:

```elixir
Logger.debug("Esta es una salida de depuración")
# Output:
# 09:00:23.345 [debug] Esta es una salida de depuración
```

También podemos configurar el nivel de registro en nuestro archivo de configuración `config.exs`:

```elixir
config :logger, level: :debug
```

Con esta configuración, todas las salidas de depuración se imprimirán en la consola.

## Vea también
- [Documentación de Elixir sobre impresión de salida de depuración](https://elixir-lang.org/getting-started/debugging.html#printing-debug-output)
- [Artículo de Medium: "Cómo utilizar IO.inspect en Elixir"](https://medium.com/@anamos/how-to-use-io-inspect-in-elixir-1477ba4f3f3d)
- [Artículo de Elixir School sobre el módulo de registro de Elixir](https://elixirschool.com/en/lessons/advanced/logging/)