---
title:                "Elixir: Comenzando un nuevo proyecto"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Empezar un nuevo proyecto en Elixir puede ser una excelente opción para aquellos que buscan un lenguaje de programación funcional y altamente escalable. Con una sintaxis fácil de aprender y una comunidad activa, Elixir es una herramienta poderosa para crear aplicaciones robustas y eficientes.

## Cómo hacerlo

Antes de comenzar a escribir código en Elixir, deberás asegurarte de tener instalado el runtime de Erlang y el sistema de construcción Mix. Una vez que tengas todo configurado, puedes seguir estos pasos para iniciar tu nuevo proyecto:

1. Crea una nueva carpeta para tu proyecto.
2. Abre una terminal y navega hasta la carpeta que acabas de crear.
3. Ejecuta el comando `mix new nombre_del_proyecto` para generar una nueva estructura de proyecto con algunos archivos predefinidos.
4. Navega a través de los archivos y edítalos según sea necesario para satisfacer tus necesidades.
5. Para ejecutar tu proyecto, simplemente escribe `mix run` en la terminal.
6. ¡Empieza a escribir tu código en Elixir y haz que tu proyecto cobre vida!

Aquí hay un ejemplo básico de cómo imprimir "¡Hola mundo!" en la terminal usando Elixir:

```Elixir
defmodule Hola do
  def mundo do
    IO.puts "¡Hola mundo!"
  end
end

Hola.mundo()
```

La salida de este código sería:

```
¡Hola mundo!
```

## Profundizando

Al iniciar un nuevo proyecto en Elixir, es importante entender algunos conceptos clave como la inmutabilidad, la concurrencia y el patrón de diseño de OTP (Open Telecom Platform). Estos son los pilares sobre los que se construye Elixir, por lo que conocerlos te ayudará a escribir un código más eficiente y escalable.

La inmutabilidad se refiere a la idea de que los datos no deben cambiar una vez que se han creado, en lugar de eso, se deben crear nuevas versiones de los mismos. Esto ayuda a prevenir errores y hace que el código sea más fácil de razonar.

La concurrencia es una característica clave de Elixir que permite que varias tareas se ejecuten simultáneamente en un solo procesador. Esto se logra a través del uso de procesos livianos, que son mucho más eficientes que los hilos de los sistemas operativos. Esto permite que las aplicaciones escritas en Elixir manejen grandes cantidades de tareas de manera eficiente.

El patrón de diseño de OTP es la forma en que Elixir organiza y estructura aplicaciones para asegurar que sean estables y escalables. Utiliza conceptos como la supervisión, que permite que una aplicación se recupere de errores de manera automática, y los GenServers, que son procesos de alto nivel que se encargan de procesar tareas específicas.

## Ver también

- [Documentación oficial de Elixir](https://elixir-lang.org/docs.html)
- [Elixir School en español](https://elixirschool.com/es/)
- [Lista de recursos de aprendizaje de Elixir en español](https://github.com/edgar/awesome-elixir-es)