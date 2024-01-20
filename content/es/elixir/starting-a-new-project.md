---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Iniciar un nuevo proyecto en la programación consiste en crear un espacio vacío en el cual tendremos la libertad de construir nuestras ideas creadas desde cero. Programamos para convertir esas ideas en realidad y resolver problemas con nuestras propias soluciones.

## ¿Cómo se hace?

Crear un nuevo proyecto de Elixir es tan simple como ejecutar un comando. Vamos a usar Mix, una herramienta que viene con Elixir que permite crear, compilar, y probar proyectos.

```Elixir
mix new nombre_del_proyecto
```

Este comando creará un nuevo directorio `nombre_del_proyecto` con una estructura básica de proyecto de Elixir. Ejecute `ls nombre_del_proyecto` para explorar los archivos generados.

```Elixir
ls nombre_del_proyecto
```

Verá algo parecido a esto:

```Elixir
README.md  config  lib  mix.exs  test
```

## Inmersión profunda

El comando `mix new` proviene de una herramienta llamada Mix, que forma parte de Elixir desde su lanzamiento en 2011. Aunque existen alternativas a Mix, como es el caso de las herramientas de construcción Nerves y Bakeware, Mix es la opción predeterminada más popular debido a su simplicidad y fácil integración con Elixir.

Además de crear proyectos, Mix puede administrar dependencias, compilar archivos de código, ejecutar pruebas y mucho más. Lo que hace esencialmente es tomar la configuración especificada en `mix.exs`, un archivo generado en cada nuevo proyecto de Mix, y ejecutar acciones en base a esa configuración.

Entender cómo se genera un proyecto en Elixir nos proporciona una visión más profunda de lo que ocurre detrás de escena cuando trabajamos en nuestros propios proyectos.

## Ver También

* Documentación oficial de Elixir: https://elixir-lang.org/docs.html
* Guia de mix y OTP de Elixir: http://elixir-lang.github.io/getting-started/mix-otp/introduction-to-mix.html
* Foro de Elixir: https://elixirforum.com/

Los foros y la documentación oficial son recursos invaluables para aprender más sobre la programación en Elixir, y te animo a explorarlos mientras te adentras en tu próximo proyecto.