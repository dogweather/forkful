---
title:                "Comprobando si existe un directorio"
html_title:           "Fish Shell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Si estás trabajando en un proyecto en el que necesitas manejar diferentes directorios, es posible que en algún momento necesites verificar si un directorio específico existe antes de realizar alguna acción. Ya sea para asegurarte de que una ruta es válida o para evitar errores en tu código, es importante saber cómo verificar la existencia de un directorio en Fish Shell.

## Cómo hacerlo

Fish Shell tiene un comando interno llamado `test` que nos permite realizar diferentes comprobaciones, incluyendo la verificación de la existencia de un directorio. Podemos usarlo de la siguiente manera:

```Fish Shell
test -d <directorio>
```

Si el directorio existe, este comando devolverá `true`, de lo contrario, devolverá `false`. También podemos utilizar la sintaxis de comillas dobles para referirnos a un directorio que contenga espacios en su nombre:

```Fish Shell
test -d "<directorio>"
```

Por ejemplo, si queremos verificar si el directorio "documentos" existe en nuestra carpeta de usuario, podemos usar el siguiente comando:

```Fish Shell
test -d "~/documentos"
```

Si el directorio existe, obtendremos como resultado `true`. Si lo combinamos con un condicional `if`, podemos realizar ciertas acciones según el resultado de la comprobación.

## Profundizando

Si queremos saber más sobre la verificación de la existencia de un directorio en Fish Shell, podemos revisar la sección de documentación correspondiente. Allí encontraremos información sobre otros comandos y opciones que pueden ser útiles en diferentes situaciones.

También podemos utilizar la sugerencia de auto-completar de Fish Shell para obtener una lista de los directorios existentes en nuestra ubicación actual. Simplemente escribimos `test -d` seguido de un espacio y luego presionamos la tecla Tab. Esto nos mostrará una lista de los directorios existentes y nos ayudará a escribir la ruta correctamente.

## Ver también

- Documentación oficial sobre test: https://fishshell.com/docs/current/cmds/test.html
- Listas de directorios en Fish Shell: https://fishshell.com/docs/current/cmds/complete.html#dirnames