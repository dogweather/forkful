---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Iniciar un nuevo proyecto es donde un programador define una idea inicial y la transforma en código. Los programadores lo hacemos para resolver problemas, innovar o simplemente aprender algo nuevo.

## Cómo hacerlo:

Para iniciar un nuevo proyecto en Python, es crucial organizar de manera efectiva los archivos y las estructuras de directorio.

Primero,, crea un nuevo directorio para el proyecto.

```Python
mkdir nuevo_proyecto
cd nuevo_proyecto
```

Luego inicia un nuevo repositorio de Git. Git es una herramienta de control de versiones que permite realizar un seguimiento de los cambios en el código.

```Python
git init
```

Organiza las carpetas. Por ejemplo:

```Python
mkdir src
mkdir tests
touch README.md
```
## Inmersión Profunda

Históricamente, los proyectos no siempre han tenido una estructura de directorio adecuada, lo que a menudo lleva a un caos en el código. Con el tiempo, los programadores han aprendido la importancia de una buena estructura de archivos y directorios. 

Como alternativas al enfoque manual anterior, existen herramientas como Cookiecutter que pueden generar una estructura de proyecto en Python para ti.

La implementación de nuevos proyectos requiere consideración y organización. No sólo se trata de empezar a escribir código. Necesitas pensar en las funcionalidades, tests y cómo otros programadores interactuarán con tu código.

## Ver También

1. [Guía de estilo Python oficial (PEP 8)](https://www.python.org/dev/peps/pep-0008/)
2. [Herramienta Cookiecutter](https://github.com/cookiecutter/cookiecutter)
3. [Git - Herramienta de Control de Versión](https://git-scm.com/) 

Recuerda, organiza tus proyectos de manera efectiva ahorra tiempo en un futuro. ¡Feliz codificación!