---
title:                "Comenzando un nuevo proyecto"
html_title:           "Elixir: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Iniciar un nuevo proyecto en Elixir significa crear una base para desarrollar una aplicación o software utilizando este lenguaje de programación. Los programadores lo hacen para aprovechar las características únicas de Elixir, como el manejo de concurrencia y la escalabilidad, para crear aplicaciones eficientes y confiables.

## Cómo hacerlo:

```
Elixir.new
```

Este comando crea una nueva aplicación en Elixir con una estructura básica de carpetas y archivos. También instala todas las dependencias necesarias para ejecutar la aplicación.

```
mix new my_app
```

Si deseas especificar un nombre para tu aplicación, reemplaza "my_app" con el nombre deseado en el comando anterior.

```
cd my_app
```

Este comando te lleva al directorio de tu nueva aplicación, donde puedes comenzar a escribir tu código en Elixir.

## En profundidad:

Elixir se basa en el lenguaje de programación Erlang, el cual fue desarrollado para crear sistemas distribuidos y tolerantes a fallos. Al aprovechar la infraestructura de Erlang, Elixir hereda estas capacidades y ofrece una sintaxis más amigable y un conjunto de herramientas más modernas. Alternativas a Elixir incluyen otros lenguajes de programación como Rust o Go, pero cada uno tiene sus propias fortalezas y debilidades.

Al iniciar un nuevo proyecto en Elixir, también tendrás la opción de utilizar un framework web como Phoenix. Este framework proporciona una estructura adicional para crear aplicaciones web en un entorno Elixir, lo que simplifica la creación de servidores web y aplicaciones de tiempo real.

## Ver también:

- Documentación de Elixir: https://elixir-lang.org/getting-started/introduction.html
- Tutorial de Phoenix: https://hexdocs.pm/phoenix/up_and_running.html#content
- Comparación entre Elixir y otros lenguajes: https://stackshare.io/stackups/elixir-vs-go-vs-rust