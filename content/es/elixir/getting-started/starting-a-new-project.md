---
date: 2024-01-20 18:03:11.466768-07:00
description: "Iniciar un nuevo proyecto en Elixir es como abrir un lienzo en blanco\
  \ para pintar tu obra maestra con c\xF3digo. Los programadores lo hacen para resolver\u2026"
lastmod: '2024-02-25T18:49:55.260011-07:00'
model: gpt-4-1106-preview
summary: "Iniciar un nuevo proyecto en Elixir es como abrir un lienzo en blanco para\
  \ pintar tu obra maestra con c\xF3digo. Los programadores lo hacen para resolver\u2026"
title: Iniciando un nuevo proyecto
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Iniciar un nuevo proyecto en Elixir es como abrir un lienzo en blanco para pintar tu obra maestra con código. Los programadores lo hacen para resolver problemas, experimentar con ideas nuevas o simplemente para mejorar sus habilidades en la creación de software estructurado y mantenible.

## Cómo Iniciar:
Primero, asegúrate que tienes Elixir y Mix, su herramienta de construcción, instalados. Para empezar un proyecto, abre tu terminal y ejecuta:

```elixir
mix new mi_app
```

Eso creará una estructura de directorio estándar para tu aplicación con todo lo necesario para empezar. La salida se verá algo así:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/mi_app.ex
* creating test
* creating test/test_helper.exs
* creating test/mi_app_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd mi_app
    mix test

Run "mix help" for more commands.
```

## Deep Dive
Elixir es un lenguaje de programación moderno diseñado para sistemas escalables y mantenibles. Sus raíces están inspiradas en Erlang, creado por Ericsson en 1986 para telecomunicaciones. Mix, la herramienta que usamos para crear proyectos, es más que un generador de estructuras: gestiona dependencias, compila el código, y ejecuta pruebas.

Alternativas a `mix new` podrían incluir la creación manual de archivos y configuraciones, pero es innecesario dado que Mix simplifica y estandariza este proceso. Además, el uso de `--sup` al crear un proyecto genera un esqueleto para una aplicación supervisada, esencial para sistemas robustos y autoreparables. Ejemplo:

```elixir
mix new mi_app --sup
```

Desde el punto de vista de implementación, cuando ejecutas `mix new`, Mix crea la estructura de directorio basada en el nombre que proporcionas y genera archivos de configuración mínimos, necesarios para correr y probar tu aplicación.

## Véase También
- Documentación oficial de Mix: https://hexdocs.pm/mix/Mix.html
- Elixir en GitHub: https://github.com/elixir-lang/elixir
- Guía de inicio rápido de Elixir: https://elixir-lang.org/getting-started/introduction.html
- Erlang/OTP: http://www.erlang.org/
