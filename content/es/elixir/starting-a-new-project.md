---
title:                "Iniciando un nuevo proyecto"
html_title:           "Elixir: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando comenzar un nuevo proyecto de programación, Elixir es una excelente opción. Es un lenguaje de programación funcional que se ejecuta en la máquina virtual de Erlang y está diseñado para construir aplicaciones escalables y concurrentes. Además, Elixir es fácil de aprender y ofrece una amplia gama de herramientas y librerías para ayudarte a construir tu proyecto de manera eficiente.

## Cómo comenzar

Antes de comenzar a codificar, debes instalar Elixir en tu sistema. Puedes encontrar instrucciones detalladas en el [sitio web oficial de Elixir](https://elixir-lang.org/install.html). Una vez que tengas Elixir instalado, puedes utilizar el comando `mix new` para crear una nueva aplicación.

```
Elixir mix new my_app
```

Esto creará una estructura básica de aplicación en el directorio `my_app` con archivos como `mix.exs` y `lib/my_app.ex`. Además, Elixir utiliza el [patrón de diseño de supervisor](https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html) para manejar errores y supervisar la ejecución de tus aplicaciones.

## Profundizando

Al comenzar un nuevo proyecto en Elixir, es importante tener en cuenta cómo se estructuran las aplicaciones. Puedes crear módulos y funciones utilizando el operador `def` y separarlos en diferentes módulos si es necesario. Por ejemplo:

```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end
```

Esto creará un módulo llamado `Calculator` con una función de `add` que suma dos números. Para llamar a esta función, puedes utilizar el operador `.` y pasar los argumentos necesarios:

```Elixir
Calculator.add(2, 4)
```

Además, Elixir tiene un sistema de tipos dinámico y utiliza el concepto de "patrones" para hacer coincidir y manejar datos. Puedes profundizar en estos conceptos y más en la [documentación oficial de Elixir](https://elixir-lang.org/getting-started/basic-types.html).

## Ver también

Si estás interesado en aprender más sobre Elixir y cómo comenzar un nuevo proyecto, aquí tienes algunos recursos adicionales:

- [Getting started with Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir School - A community-driven learning platform](https://elixirschool.com/)
- [Awesome Elixir - A curated list of awesome Elixir libraries](https://github.com/h4cc/awesome-elixir)

¡Ahora estás listo para comenzar tu propio proyecto en Elixir! ¡Diviértete explorando este increíble lenguaje de programación funcional!