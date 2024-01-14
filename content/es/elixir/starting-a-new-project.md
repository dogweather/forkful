---
title:    "Elixir: Comenzando un nuevo proyecto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Iniciar un nuevo proyecto en Elixir puede brindar numerosos beneficios, como la capacidad de crear aplicaciones altamente escalables y tolerantes a fallos. Además, Elixir cuenta con una sintaxis intuitiva y una comunidad activa que ofrece numerosos recursos y ayuda para el desarrollo de proyectos.

## Cómo

Para comenzar un nuevo proyecto en Elixir, primero debes asegurarte de tener instalado el lenguaje en tu sistema. Luego, puedes crear una nueva aplicación utilizando el comando `mix new nombre_de_tu_app`. Este comando creará automáticamente la estructura básica de tu aplicación.

A continuación, puedes editar el archivo `mix.exs` para añadir dependencias externas y configurar tu aplicación. Luego, puedes comenzar a escribir tu código en los archivos `.ex` utilizando la sintaxis de Elixir. Por ejemplo:

```Elixir
defmodule Saludo do
  def hola(nombre) do
    IO.puts "Hola #{nombre}!"
  end
end

Saludo.hola("Juan") # salida: Hola Juan!
```

Elixir también cuenta con un poderoso sistema de concurrencia llamado Erlang VM, que te permite ejecutar múltiples procesos de forma eficiente. Puedes utilizar este sistema para crear aplicaciones robustas y tolerantes a fallos. Por ejemplo:

```Elixir
defmodule Calculadora do
  def sumar(x, y) do
    proceso = spawn(fn -> IO.puts "Calculando la suma de #{x} y #{y}..." end)
    resultado = x + y
    send(proceso, resultado)
  end
end

Calculadora.sumar(5, 7) # salida: Calculando la suma de 5 y 7...
                        #        12
```

## Deep Dive

Además de su elegante sintaxis y su sistema de concurrencia, Elixir también cuenta con una gran cantidad de herramientas y frameworks para facilitar el desarrollo de aplicaciones. Por ejemplo, Phoenix es un framework web basado en Elixir que te permite crear fácilmente aplicaciones web de alta calidad.

También tienes la opción de utilizar Ecto, un mapeador de objetos relacionales que te permite interactuar con bases de datos utilizando el poderoso lenguaje de consulta de Elixir. Esto facilita la manipulación de datos en tus aplicaciones.

En cuanto a herramientas de testing, puedes utilizar ExUnit para escribir pruebas automatizadas y garantizar la calidad de tu código.

Otra gran ventaja de Elixir es su capacidad de integración con Erlang, lo que te permite aprovechar la amplia variedad de bibliotecas y aplicaciones disponibles en el ecosistema de Erlang.

Con todas estas herramientas y características, iniciar un nuevo proyecto en Elixir es una gran opción para aquellos que buscan desarrollar aplicaciones robustas y escalables de manera eficiente.

## Vea También

- [Documentación oficial de Elixir](https://elixir-lang.org/docs.html)
- [Phoenix Framework](https://www.phoenixframework.org/)
- [Ecto](https://hexdocs.pm/ecto/Ecto.html)
- [ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Erlang](https://www.erlang.org/)