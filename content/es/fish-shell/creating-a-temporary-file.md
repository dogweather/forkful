---
title:                "Creando un archivo temporal"
html_title:           "Fish Shell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear archivos temporales es útil cuando necesitamos almacenar información temporalmente en nuestro sistema. Puede ser útil para aplicaciones que requieren guardar datos temporales durante la ejecución del programa, o para realizar pruebas y experimentos sin comprometer los archivos permanentes.

## Cómo hacerlo

En Fish Shell, podemos crear un archivo temporal utilizando el comando `mktemp`. Por ejemplo, si queremos crear un archivo temporal llamado "temp.txt" en nuestro directorio actual, podemos hacerlo de la siguiente manera:

```Fish Shell
mktemp temp.txt
```

Esto creará un archivo llamado "temp.txt" en el directorio actual y nos dará su ruta completa, que podemos utilizar para acceder y trabajar con ese archivo.

Si queremos crear un archivo temporal dentro de un directorio específico, podemos especificar la ruta al directorio como argumento para el comando `mktemp`:

```Fish Shell
mktemp ~/Documentos/temp.txt
```

También podemos utilizar la opción `-d` para crear un directorio temporal en lugar de un archivo temporal:

```Fish Shell
mktemp -d ~/Documentos/temporal/
```

### Deep Dive

El comando `mktemp` es en realidad un backend para el comando `mktemp.fish`, que es la versión de Fish Shell de este comando. Esto significa que podemos usar `mktemp.fish` en lugar de `mktemp` si queremos acceder a sus opciones y funcionalidades adicionales. Por ejemplo, el comando `mktemp.fish` nos permite especificar un patrón para el nombre del archivo temporal que se generará. Esto es útil si queremos que el archivo temporal siga un patrón específico para facilitar su identificación y eliminación posteriormente.

Otra diferencia entre `mktemp` y `mktemp.fish` es que `mktemp.fish` nos permite especificar un prefijo para el nombre del archivo temporal. Esto significa que podemos usar nombres más descriptivos para nuestros archivos temporales, lo que facilita su comprensión y gestión.

## Ver También

- [Página oficial de Fish Shell](https://fishshell.com/)
- [Documentación de Fish Shell sobre el comando `mktemp`](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Tutorial de Fish Shell en español](https://shellsystem.es/tutorial-fish-shell-el-editor-de-comandos-que-no-conoces/)