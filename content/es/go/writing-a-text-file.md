---
title:                "Escribiendo un archivo de texto"
html_title:           "Go: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Go

Escribir un archivo de texto en Go puede ser una forma eficiente de almacenar y compartir información en un formato legible para los humanos. Además, puede ser una herramienta útil en procesos de automatización de tareas.

## Cómo hacerlo

La forma más sencilla de escribir un archivo de texto en Go es utilizando la función `WriteFile` de la librería estándar `io/ioutil`. Esta función toma tres argumentos: la ruta del archivo, los datos a escribir y los permisos de escritura. Por ejemplo:

```
package main

import (
    "io/ioutil"
    "log"
)

func main() {
    // Crear un archivo llamado "ejemplo.txt" con permisos de escritura
    err := ioutil.WriteFile("ejemplo.txt", []byte("Este es un ejemplo de texto en Go"), 0644)
    if err != nil {
        log.Fatal(err)
    }
}
```

Este código creará un archivo llamado "ejemplo.txt" en el mismo directorio del archivo Go, con el texto "Este es un ejemplo de texto en Go". El valor numérico "0644" representa los permisos de escritura para el propietario y el grupo del archivo, así como los permisos de lectura para otros usuarios.

## Inmersión en detalle

Al escribir un archivo de texto en Go, es importante tener en cuenta los diferentes permisos de escritura que se pueden otorgar. Por ejemplo, los permisos "0644" mencionados anteriormente significan que el propietario tiene permisos de escritura y lectura, el grupo solo tiene permisos de lectura y los demás usuarios solo tienen permisos de lectura. Sin embargo, estos permisos pueden ser cambiados según las necesidades del usuario.

## Ver también

- [Go Docs: ioutil.WriteFile](https://golang.org/pkg/io/ioutil/#WriteFile)
- [Go Docs: Permisos de archivos](https://golang.org/pkg/io/fs/#FileMode)