---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comenzar un nuevo proyecto de programación significa poner en marcha una nueva idea desde cero. Los programadores lo hacen para resolver problemas específicos, implementar nuevas funcionalidades o experimentar con nuevas tecnologías.

## ¿Cómo se hace?
Aquí te muestro cómo iniciar un nuevo proyecto con Leiningen, una popular herramienta de Clojure para la gestión de proyectos. 

Primero, debes instalar Leiningen. Luego, para crear un nuevo proyecto, usa la pestaña `new`:

```Clojure
lein new mi-proyecto
```
Esto generará una nueva carpeta llamada `mi-proyecto` con toda la estructura necesaria para comenzar a codificar.

Para ejecutar tu proyecto, debes ubicarte en la carpeta del proyecto e introducir el comando `run`:

```Clojure
cd mi-proyecto
lein run
```
El resultado será algo así:

```
Hello, World!
```
Por supuesto, puedes modificar el código a tu gusto.

## Un vistazo profundo

Empezar un nuevo proyecto con Leiningen es fácil y rápido, pero no siempre fue así. Históricamente, antes de tener este tipo de herramientas, iniciar un nuevo proyecto era un proceso tedioso y complejo.

Existen alternativas a Leiningen. Para Clojure, Boot es otro gestor de proyectos popular, y para otros lenguajes de programación existen herramientas similares como Maven para Java o Cargo para Rust.

Los detalles de implementación al iniciar un nuevo proyecto con Leiningen incluyen la generación de un archivo `project.clj` que define las dependencias del proyecto y las tareas asociadas con él. Además, Leiningen genera automáticamente una estructura de directorios y archivos ideal para proyectos de Clojure.

## Ver También
1. [Leiningen: para gestión de proyectos](https://leiningen.org/)
2. [Boot: alternativa a Leiningen](https://boot-clj.com/)
3. [Documentación de Clojure](https://clojure.org/guides/getting_started)
4. [La Guía de estilo de Clojure](https://github.com/bbatsov/clojure-style-guide)