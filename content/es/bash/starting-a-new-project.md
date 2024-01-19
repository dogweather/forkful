---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Emprender un nuevo proyecto en programación significa comenzar a desarrollar una nueva aplicación o sistema desde cero. Los programadores hacen esto para crear soluciones a problemas o desarrollar nuevas funcionalidades.

## Cómo hacerlo:

Por ejemplo, en Bash puedes crear un nuevo directorio para cada proyecto. A menudo es una buena idea inicializar un repositorio git en el directorio del proyecto.

```Bash
$ mkdir MiNuevoProyecto
$ cd MiNuevoProyecto
$ git init
```

Esto genera:

```Bash
Inicializado repositorio Git vacío en /ruta/a/tu/proyecto/.git/
```

## Análisis en profundidad:

Históricamente, un proyecto se podría comenzar de cualquier manera, pero estamos centrados en una forma eficiente de hacerlo con Bash. Hay otras alternativas como GUIs y IDEs, pero Bash es simple, efectivo y ampliamente disponible.
    
En lo que respecta a detalles de implementación, el uso de control de versiones como git es esencial. Además de permitir hacer un seguimiento de los cambios en el código, facilita la colaboración y puede ayudar a prevenir la pérdida de trabajo.

## Ver también:

Para aprender más sobre el bash scripting, puedes verificar estos recursos:

- [Guía avanzada de Bash-Scripting](https://www.tldp.org/LDP/abs/html/index.html) (en inglés)
- [Git - La guía sencilla](http://rogerdudler.github.io/git-guide/index.es.html)
- [Pensando en git](https://maryrosecook.com/blog/post/git-from-the-inside-out) (en inglés)