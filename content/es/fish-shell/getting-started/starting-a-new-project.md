---
date: 2024-01-20 18:03:35.233181-07:00
description: "C\xF3mo hacerlo: Salida esperada (en caso de que se a\xF1ada un `echo\
  \ \"Hola Mundo\"` dentro de main.fish)."
lastmod: '2024-04-05T21:54:00.854774-06:00'
model: gpt-4-1106-preview
summary: "Salida esperada (en caso de que se a\xF1ada un `echo \"Hola Mundo\"` dentro\
  \ de main.fish)."
title: Iniciando un nuevo proyecto
weight: 1
---

## Cómo hacerlo:
```Fish Shell
# Crear un nuevo directorio y moverse a él
mkdir mi_proyecto && cd mi_proyecto

# Inicializar un repositorio Git
git init

# Crear un archivo README básico
echo "# Mi Proyecto" > README.md

# Crear un directorio para el código fuente
mkdir src

# Crear un script de Fish como 'main.fish' en 'src'
echo "#!/usr/bin/env fish\n\n# Punto de entrada de mi aplicación" > src/main.fish
chmod +x src/main.fish

# Ejemplo de cómo ejecutar tu script
fish src/main.fish
```
Salida esperada (en caso de que se añada un `echo "Hola Mundo"` dentro de main.fish):
```
Hola Mundo
```

## Profundización
Antes de Fish Shell, se utilizaba Bash o Zsh para estas tareas, pero Fish ofrece una sintaxis simplificada y características amigables como la autocompletación y el coloreado de sintaxis. Al configurar un nuevo proyecto, es importante también pensar en la estructura. Aparte de `src`, podrías querer directorios como `docs`, para documentación, o `tests`, para pruebas. Hablando de implementación, asegúrate de que tu editor de texto esté configurado para trabajar con Fish y considera si necesitas gestionar dependencias o configurar entornos virtuales para tu proyecto.

## Ver También
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Git Basics](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)
- [GitHub's Hello World](https://guides.github.com/activities/hello-world/)
- [Project Directory Structure Best Practices](https://softwareengineering.stackexchange.com/questions/338597/folder-by-type-or-folder-by-feature)
