---
title:    "Go: Leyendo un archivo de texto"
keywords: ["Go"]
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Go?

La lectura de archivos de texto en Go es una tarea común en el desarrollo de software. Puede usar esto para leer datos almacenados en un archivo y luego manipularlos en su programa. En este blog post, aprenderá cómo leer un archivo de texto en Go y cómo puede ser beneficioso para su proyecto.

## Cómo hacerlo

Para leer un archivo de texto en Go, primero debemos importar el paquete "os". Este paquete proporciona funciones para interactuar con el sistema operativo, incluidas la lectura y escritura de archivos. Luego, usamos la función "os.Open()" para abrir el archivo de texto que queremos leer.

Una vez que tenemos el archivo abierto, podemos leer su contenido usando la función "bufio.NewReader()". Esta función nos permite crear un lector que nos permitirá acceder a los datos del archivo línea por línea. Luego, podemos usar el método "ReadString()" para leer cada línea del archivo y almacenarla en una variable.

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("archivo.txt") // Abrir el archivo
	if err != nil {
		fmt.Println(err)
	}
	defer file.Close() // Cerrar el archivo al finalizar el programa

	scanner := bufio.NewReader(file) // Crear un lector para el archivo
	for {
		line, err := scanner.ReadString('\n') // Leer una línea del archivo
		if err == io.EOF { // Cuando llegamos al final del archivo, salir del bucle
			break
		} else if err != nil {
			fmt.Println(err)
		}
		fmt.Println(line) // Imprimir la línea en la consola
	}
}
```

## Profundizando

La función "ReadString()" en el ejemplo anterior toma un parámetro llamado "delimitador" que se usa para determinar dónde se debe detener la lectura de cada línea. En nuestro caso, usamos '\n' ya que queremos leer la línea completa antes de que se encuentre un salto de línea.

También puede usar otras funciones para leer datos de un archivo de texto en Go. Por ejemplo, puede usar la función "ioutil.ReadFile()" para leer todo el archivo y almacenarlo en una variable de una sola vez. Sin embargo, esta función no es adecuada para archivos grandes ya que puede consumir mucha memoria.

## Ver también

- Documentación oficial de lectura y escritura de archivos en Go: https://golang.org/pkg/os/file/
- Ejemplos de código para leer archivos de texto en Go: https://www.golangprograms.com/how-to-read-file-line-by-line-in-golang.html