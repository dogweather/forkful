---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Iniciar un nuevo proyecto significa comenzar a desarrollar un nuevo producto o aplicación desde cero. Los programadores lo hacen para crear soluciones a problemas específicos o para desarrollar nuevas ideas.

## Cómo hacerlo: 

Aquí está el código que necesitarás para empezar un nuevo proyecto en Gleam:

```Gleam
gleam new hola_mundo
cd hola_mundo
```
Después de estos dos sencillos comandos, tendrás como resultado un nuevo proyecto de Gleam llamado "hola_mundo".

```Gleam
src
|_ hola_mundo.gleam
rebar.config
.gitignore
README.md
```
Estos son los integrantes de tu nuevo proyecto. 'src' es el directorio donde estará tu código Gleam.

## Dive Profundo: 

Un poco de historia, antes de que Gleam existiera, Erlang y Elixir eran las únicas lenguas habladas en la máquina virtual de Erlang (BEAM). Gleam pretende combinar la seguridad de tipos con la concurrencia masiva y tolerante a fallos de Erlang. En cuanto a las alternativas, puedes utilizar las mencionadas Erlang y Elixir u otras lenguas BEAM como LFE.

En términos de detalles de implementación, al iniciar un nuevo proyecto, Gleam genera un número de archivos y directorios para establecer una estructura básica del proyecto. Esto incluye un archivo de configuración de Rebar y un directorio /src que contendrá el código fuente de Gleam para el nuevo proyecto.

## Véase también:

- Documentación oficial de Gleam: https://gleam.run/
- Comparación de Gleam con otros idiomas BEAM: https://gleam.run/faq/#how-does-gleam-compare-to-language-x