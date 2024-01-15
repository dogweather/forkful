---
title:                "Creando un archivo temporal"
html_title:           "Go: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
Crear un archivo temporal es una práctica común en la programación para guardar datos temporalmente y evitar conflictos en el manejo de datos.

## Cómo hacerlo
La creación de un archivo temporal en Go es muy sencilla. Primero, importamos el paquete "io/ioutil", el cual nos permite trabajar con archivos. Luego, utilizamos la función "ioutil.TempFile()" para crear un archivo temporal con el nombre y extensión que deseemos. Finalmente, podemos escribir o leer datos en este archivo utilizando las funciones proporcionadas por el paquete "ioutil". A continuación, se muestra un ejemplo de cómo crear un archivo temporal y escribir texto en él.

```Go
import "io/ioutil"

// Crea un archivo temporal con extensión .txt
archivoTemporal, err := ioutil.TempFile("", "tempfile-*.txt")
if err != nil {
    log.Fatal(err)
}
defer archivoTemporal.Close()

// Escribe texto en el archivo temporal
texto := []byte("Hola, este es un archivo temporal creado con Go.")
if _, err := archivoTemporal.Write(texto); err != nil {
    log.Fatal(err)
}
```
El nombre del archivo temporal se genera automáticamente de acuerdo al patrón proporcionado como segundo argumento en la función "ioutil.TempFile()". En este caso, "tempfile-*.txt" significa que el nombre del archivo tendrá el prefijo "tempfile-" seguido de un número aleatorio y la extensión ".txt".

Para leer datos de un archivo temporal, se pueden utilizar las funciones "ioutil.ReadFile()" o "ioutil.ReadAll()". A continuación, se muestra un ejemplo de cómo leer el texto guardado en el archivo temporal creado anteriormente.

```Go
// Lee el contenido del archivo temporal
contenido, err := ioutil.ReadFile(archivoTemporal.Name())
if err != nil {
    log.Fatal(err)
}

// Imprime el contenido en la consola
fmt.Println(string(contenido))
```

## Profundizando
Ahora que sabemos cómo crear y trabajar con archivos temporales en Go, es importante mencionar que estos archivos se eliminan automáticamente una vez que el programa finaliza su ejecución. Sin embargo, si deseamos eliminar el archivo temporal antes de que el programa termine, podemos utilizar la función "os.Remove()", proporcionándole como argumento el nombre del archivo. Por ejemplo:

```Go
// Elimina el archivo temporal antes de que el programa finalice
err := os.Remove(archivoTemporal.Name())
if err != nil {
    log.Fatal(err)
}
```

Es importante tener en cuenta que si el programa falla antes de eliminar el archivo temporal, éste permanecerá en el sistema. Por lo tanto, es una buena práctica utilizar la función "defer" al crear el archivo temporal para asegurarnos de que se elimine al finalizar el programa, incluso en caso de errores.

## Ver también
- Documentación oficial de Go sobre el paquete "io/ioutil": https://golang.org/pkg/io/ioutil/
- Tutorial de creación de archivos temporales en Go: https://gobyexample.com/writing-files
- Ejemplos de uso de funciones "ioutil" en la creación de archivos en Go: https://golangdocs.com/create-a-file-in-golang