---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Leer un archivo de texto en Go

## ¿Qué y Por qué?
Leer un archivo de texto en programación implica extraer datos escritos en un archivo. Los programadores lo hacemos para acceder y manipular datos almacenados fuera del código fuente.

## ¿Cómo?
Vamos a leer un archivo de texto en Go usando el paquete `os` y `bufio`.

```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		log.Fatalf("Error al abrir el archivo: %s", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```
Si `test.txt` contiene "`Hola, mundo!`", el programa imprime:
```
Hola, mundo!
```

## Inmersión Profunda
La lectura de archivos de texto tiene una larga historia en la programación y es esencial para la manipulación de datos. Go ofrece varias formas adicionales de leer archivos, como `ioutil.ReadFile` y `file.Read`. Sin embargo, `bufio.Scanner` es generalmente preferido para leer archivos de texto debido a su eficiencia y comodidad. 

En la implementación anterior, el archivo se abre con `os.Open`, se crea un `bufio.Scanner` para el archivo y luego se lee línea por línea. Es importante cerrar el archivo después de usarlo con `defer file.Close()`. Gracias a `defer`, esta línea se ejecutará después de que se complete la función `main()`, asegurándonos de que el archivo se cierre independientemente de dónde nos salgamos del programa.

## Consulta También
Para más información, consulta:
- Los documentos oficiales de `os` y `bufio`: 
    - [os](https://pkg.go.dev/os)
    - [bufio](https://pkg.go.dev/bufio)
- Tutorial sobre cómo leer archivos en Go: 
    - [Reading a File Line by Line in Go](https://golangbot.com/read-file-line-by-line/)
- Profundizando en los `io utils` en Go: 
    - [The Go io/ioutil package](https://golangbot.com/go-ioutil/)
- Artículo sobre cómo manejar errores en Go:
    - [Error Handling in Go](https://www.alexedwards.net/blog/golang-handling-errors)