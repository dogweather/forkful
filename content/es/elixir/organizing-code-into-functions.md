---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:09:26.706817-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Organizar el código en funciones significa agrupar operaciones relacionadas en bloques reutilizables. Lo hacemos para mejorar la legibilidad y mantenibilidad, reducir la duplicación y simplificar las pruebas.

## Cómo hacerlo:
Vamos a crear una función simple en Elixir para capitalizar palabras:

```elixir
defmodule StringUtils do
  def capitalize_words(oracion) do
    oracion
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hola mundo elixir")
```
Salida:
```
Hola Mundo Elixir
```
Aquí, hemos empaquetado de manera ordenada la lógica de capitalización de palabras en una función llamada `capitalize_words`.

## En Profundidad
En Elixir, y en el ecosistema más amplio de la Máquina Virtual de Erlang, las funciones son ciudadanos de primera clase, heredando la filosofía de descomponer problemas en piezas más pequeñas, manejables y aisladas. Históricamente, este enfoque funcional tiene raíces en el cálculo lambda y los Lisps, promoviendo la filosofía de código como datos.

Las alternativas para organizar el código pueden ser el uso de macros o procesos en Elixir para tareas repetitivas o concurrentes, respectivamente. En términos de implementación, las funciones de Elixir pueden manejar el emparejamiento de patrones y recibir diferentes argumentos (aridad), otorgándoles versatilidad.

## Vea También
- [Documentación oficial de Elixir sobre funciones](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas "Programando Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
