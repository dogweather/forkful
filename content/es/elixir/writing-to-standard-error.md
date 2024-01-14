---
title:                "Elixir: Escribiendo en el error estándar"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en el estándar de error en Elixir?

Escribir en el estándar de error es una técnica útil para los programadores de Elixir. Esta práctica puede ayudar a encontrar y solucionar errores en el código de manera más eficiente, ya que los mensajes de error se muestran de manera más clara y detallada.

## Cómo hacerlo

Para escribir en el estándar de error en Elixir, simplemente usamos la función `IO.puts("mensaje de error")`. Este código imprimirá el mensaje de error en la terminal, permitiéndonos identificar rápidamente la causa del error.

```Elixir
defmodule Ejemplo do
  def dividir(x, y) do
    if (y == 0) do
      IO.puts("No se puede dividir por cero.")
    else
      x / y
    end
  end
end

IO.puts("5 dividido entre 0 es #{Ejemplo.dividir(5, 0)}")
```
La salida en la terminal será: `No se puede dividir por cero.`

Otra forma de escribir en el estándar de error es usando la macro `raise/1`, que nos permite generar un error con un mensaje personalizado.

```Elixir
defmodule Ejemplo do
  def validar_edad(edad) do
    if (edad < 18) do
      raise ArgumentError, message: "Debes ser mayor de edad para ingresar."
    end
  end
end

Ejemplo.validar_edad(16)
```

La salida en la terminal será: `** (ArgumentError) Debes ser mayor de edad para ingresar.`

## En profundidad

Además de imprimir mensajes de error, escribir en el estándar de error también nos permite enviar información detallada, como el tipo de error, la línea de código donde ocurrió y cualquier dato importante que nos ayude a detectar y solucionar el problema. También podemos enviar el error a un archivo de log o a un servicio de monitoreo para tener un registro de los errores en nuestro código.

Es importante recordar usar esta técnica con moderación, ya que una gran cantidad de mensajes de error pueden hacer que el código sea difícil de leer y mantener.

## Ver también

- [Documentación de IO](https://hexdocs.pm/elixir/IO.html)
- [Documentación de raise/1](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#raise/1)
- [Guía de Elixir para principiantes](https://coachacademy.io/learn-elixir-beginners-guide/)