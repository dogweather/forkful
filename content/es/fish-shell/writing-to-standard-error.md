---
title:                "Fish Shell: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida estándar de error es una técnica esencial en programación, ya que permite al desarrollador capturar y diagnosticar errores de manera eficiente. Si estás buscando mejorar tu flujo de trabajo en Fish Shell, aprender a escribir a la salida estándar de error es un paso importante.

## Cómo hacerlo

La sintaxis para escribir a la salida estándar de error en Fish Shell es:

```Fish Shell
echo "Este es un mensaje al error" >[2=1]
```

Esto escribirá el mensaje "Este es un mensaje al error" en la salida estándar de error, indicada por el número 2. También puedes utilizar la función `fishy_error` para escribir a la salida estándar de error de manera más sencilla, como se muestra a continuación:

```Fish Shell
fishy_error "Este es otro mensaje al error"
```

La salida resultante será:

```Fish Shell
Este es otro mensaje al error
```

## Profundizando

La salida estándar de error es diferente de la salida estándar, ya que está diseñada para mostrar mensajes y errores de una manera más visual. También puedes redirigir la salida estándar de error a un archivo utilizando la sintaxis:

```Fish Shell
comando 2> archivo_de_error
```

Esto escribirá cualquier mensaje de error generado por el comando en el archivo especificado. Además, si quieres redirigir tanto la salida estándar como la salida estándar de error a un archivo, puedes utilizar:

```Fish Shell
comando > archivo_de_salida 2>&1
```

Este comando primero escribirá la salida a un archivo de salida y luego redirigirá la salida estándar de error al mismo archivo. Esto puede ser útil si quieres guardar todos los mensajes en un solo archivo.

## Ver también

- [Documentación de Fish Shell en español](https://fishshell.com/docs/current/index.es.html)
- [Introducción a Fish Shell](https://medium.com/swlh/an-introduction-to-fish-shell-a-pleasant-command-line-shell-bc298f53ee02)
- [Guía de comandos de Fish Shell](https://guiafishing.wordpress.com/)