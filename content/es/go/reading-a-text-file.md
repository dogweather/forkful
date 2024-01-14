---
title:                "Go: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, en nuestros programas, necesitamos leer información de archivos de texto. Ya sea para procesar datos, modificar configuraciones o simplemente para mostrar información al usuario, leer un archivo de texto puede ser una tarea útil y necesaria en el mundo de la programación.

## Cómo hacerlo

Para leer un archivo de texto en Go, primero necesitamos abrirlo con el siguiente código:

```Go
archivo, err := os.Open("archivo.txt")
if err != nil {
    // manejo de error
}
```
Luego, podemos leer el contenido del archivo utilizando la función "Read" de la librería "bufio" y un bucle "for" para recorrer el archivo línea por línea:

```Go
scanner := bufio.NewScanner(archivo)
for scanner.Scan() {
    fmt.Println(scanner.Text())
}
if err := scanner.Err(); err != nil {
    // manejo de error
}
```
Este código imprimirá cada línea del archivo en la consola. Si queremos almacenar el contenido en una variable, podemos utilizar un "slice" de strings para almacenar cada línea en una posición:

```Go
var lineas []string
scanner := bufio.NewScanner(archivo)
for scanner.Scan() {
    lineas = append(lineas, scanner.Text())
}
```

## Profundizando

Si queremos tener un control más preciso sobre la forma en que leemos el archivo de texto, podemos utilizar la función "ReadString" de la librería "bufio". Esta función nos permitirá especificar un delimitador para separar cada línea del archivo. Por ejemplo, si queremos separar las palabras de una línea utilizando espacios, podemos utilizar el delimitador " ":

```Go
linea, err := reader.ReadString(" ")
```

Además, también podemos utilizar la función "ReadBytes" para leer el archivo en bloques de bytes en lugar de líneas completas. Esto puede ser útil para archivos con una gran cantidad de datos.

## Ver también

- Documentación oficial de funciones de lectura de archivos en Go: https://golang.org/pkg/bufio/
- Ejemplos prácticos de cómo leer archivos de texto en Go: https://www.calhoun.io/reading-files-in-go/
- Preguntas frecuentes sobre lectura de archivos en Go: https://www.golangprograms.com/go-program-to-read-text-file-into-string-how-to-read-open-file-handle.html