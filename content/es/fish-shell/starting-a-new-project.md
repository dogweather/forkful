---
title:                "Fish Shell: Comenzando un nuevo proyecto"
programming_language: "Fish Shell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido empezar un proyecto desde cero pero no sabes por dónde empezar? Con la ayuda de Fish Shell, puedes crear y configurar rápidamente nuevos proyectos de programación para que puedas ponerte manos a la obra en tu próximo desafío.

## Cómo hacerlo

Empezar un proyecto nuevo en Fish Shell es tan fácil como seguir estos pasos:

1. Crea un nuevo directorio para tu proyecto usando el comando `mkdir`.
2. Ingresa a ese directorio con `cd` y luego inicializa un nuevo repositorio con `git init`.
3. Crea un archivo de configuración de Fish Shell con `touch config.fish` para establecer tus preferencias de entorno.

Dentro del archivo `config.fish`, puedes personalizar tu promt con el comando `set -U fish_prompt "Mi Proyecto >"`. También puedes establecer alias útiles para comandos largos y tediosos, como `alias gc='git commit'`.

```Fish Shell
set -U fish_prompt "Mi Proyecto >"
alias gc='git commit'
```

Esto te ahorrará tiempo y esfuerzo en el futuro al escribir comandos repetitivos.

## Profundizando

Empezar un proyecto nuevo no se trata solo de configurar el entorno correcto, sino también sobre la organización y estructura del proyecto en sí. Una buena práctica es dividir tu código en diferentes archivos y carpetas, para mantenerlo limpio y fácil de navegar.

En Fish Shell, puedes crear fácilmente nuevos archivos usando `touch` y nuevas carpetas con `mkdir`. Además, puedes utilizar el comando `echo` para escribir contenido en un archivo desde la línea de comandos.

Por ejemplo, si queremos crear un archivo `main.py` en una carpeta llamada `codigo`, podríamos escribir los siguientes comandos:

```Fish Shell
mkdir codigo
touch codigo/main.py
echo "print('¡Hola Mundo!')" > codigo/main.py
```

Esto creará la carpeta y el archivo con el código escrito en el mismo comando. ¡Simple y eficiente!

## Ver también

- Documentación oficial de Fish Shell: https://fishshell.com/docs/current/index.html
- Guía de inicio rápido de Fish Shell para principiantes: https://medium.com/@jaxzin/fish-shell-quick-start-guide-for-beginners-3cfd5b0639c7
- Revisa más alias útiles para Fish Shell: https://github.com/fish-shell/awesome-fish