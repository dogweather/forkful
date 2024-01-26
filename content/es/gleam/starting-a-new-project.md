---
title:                "Iniciando un nuevo proyecto"
date:                  2024-01-20T18:03:36.040769-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Empezar un nuevo proyecto en Gleam es crear un espacio limpio para tu código. Los programadores lo hacen para organizar ideas y comenzar con una base sólida.

## Cómo Hacerlo:

Empecemos con Gleam. Asegúrate de tener Gleam instalado. Después, en tu terminal, ejecuta:

```bash
gleam new mi_proyecto_genial
```

Esto crea un nuevo proyecto en un directorio llamado `mi_proyecto_genial`. Aquí hay un ejemplo de cómo se vería el resultado:

```plaintext
Your Gleam project "mi_proyecto_genial" has been successfully created.
The project can be compiled and tested by running these commands:

    cd mi_proyecto_genial
    rebar3 compile
    rebar3 eunit
```

Estos comandos compilan y prueban tu nuevo proyecto de Gleam.

## Profundización

Historia: Gleam se creó para traer seguridad de tipos y rendimiento al ecosistema de Erlang. Comparado con Elixir o Erlang, Gleam está fuertemente tipado y compila a BEAM, la máquina virtual de Erlang.

Alternativas: Para proyectos BEAM, también puedes usar mix en Elixir o rebar3 en Erlang. Sin embargo, Gleam ofrece una sintaxis más rigurosa y checa tipos en tiempo de compilación.

Detalles de Implementación: Al crear un nuevo proyecto, Gleam configura un proyecto rebar3 por ti, incluyendo un esqueleto de directorio típico. Los archivos importantes incluyen:

- `gleam.toml`: Configuración del paquete de tu proyecto.
- `lib/`: Aquí va tu código Gleam.
- `test/`: Los tests de tu proyecto.

## Ver También

- Documentación de Gleam: https://gleam.run/books/tour/
- Repo oficial de Gleam en GitHub: https://github.com/gleam-lang/gleam
- Guía rebar3: https://www.rebar3.org/docs/getting-started
