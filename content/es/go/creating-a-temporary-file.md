---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Crear un archivo temporal es un proceso de generar un archivo que se usa para almacenar datos de forma temporal. Los programadores hacen esto para evitar congestionar la memoria con datos que solo se usan brevemente.

## Cómo:

Creemos un archivo temporal en Go:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    tempFile, err := ioutil.TempFile("temp", "sample.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    
    defer tempFile.Close()

    fmt.Printf("Se ha creado un archivo temporal: %s\n", tempFile.Name())
}
```
Cuando ejecutes este código, recibes una salida similar a:

```Go
Se ha creado un archivo temporal: /tmp/sample.123456.txt
```
## Inmersión profunda

Crear un archivo temporal ha sido un concepto básico en programación desde las primeras décadas de las computadoras para administrar eficientemente la memoria y la velocidad de procesamiento. En Go, este proceso es simplificado con funciones predefinidas.

Alternativamente, puedes usar la función `os.CreateTemp(dir, pattern string)`, esencialmente hacen lo mismo, pero ofrece una interfaz más flexible en términos de definir el patrón del nombre del archivo.

Los archivos temporales creados mediante `ioutil.TempFile` y `os.CreateTemp` no se eliminan automáticamente. Debes llamar `os.Remove()` para eliminarlos cuando ya no los necesites.

## Ver También

- Go Documentación Oficial sobre Archivos Temporales: https://golang.org/pkg/io/ioutil/#TempFile
- Introducción a la Creación de Archivos Temporales: https://golangbyexample.com/go-ioutil-tempfile-create-temp-file/
- Go Documentación Oficial sobre os CreateTemp: https://golang.org/pkg/os/#CreateTemp