---
title:                "Go: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear archivos temporales es una práctica común en la programación, particularmente en lenguajes como Go. Estas son utilizadas para almacenar información temporalmente y liberar espacio de almacenamiento una vez que ya no se necesita.

## Cómo hacerlo

Para crear un archivo temporal en Go, primero debemos importar la biblioteca "io/ioutil". Luego, podemos usar la función "ioutil.TempFile()" para crear un archivo temporal. Esta función toma dos argumentos: la ruta donde queremos guardar el archivo temporal y un prefijo que se usará para nombrar el archivo.

```
import (
    "fmt"
    "io/ioutil"
)

func main() {
    archivoTemporal, err := ioutil.TempFile("", "temp") // "temp" será el prefijo del nombre del archivo temporal
    if err != nil {
        fmt.Println("Error al crear el archivo temporal:", err)
        return
    }
    defer archivoTemporal.Close()
    fmt.Println("Se creó el archivo temporal:", archivoTemporal.Name())
}
```

El ejemplo anterior creará un archivo temporal en la carpeta por defecto del sistema operativo y lo nombrará como "tempXXXXXX", donde las "X" son caracteres aleatorios. El archivo se cerrará automáticamente al finalizar su uso.

## Profundizando

La función "ioutil.TempFile()" crea un archivo temporal con permisos de lectura y escritura. Pero a veces, es posible que deseemos cambiar los permisos del archivo o especificar una carpeta específica para almacenarlo. En ese caso, podemos usar la función "ioutil.TempDir()" para crear una carpeta temporal y luego usar la función "os.Create()" para crear un archivo dentro de ella.

```
import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    carpetaTemporal, err := ioutil.TempDir("", "temp")
    if err != nil {
        fmt.Println("Error al crear la carpeta temporal:", err)
        return
    }
    defer os.RemoveAll(carpetaTemporal) // Borrar la carpeta temporal al finalizar
    archivoTemporal, err := os.Create(carpetaTemporal + "/nuevoArchivo.txt")
    if err != nil {
        fmt.Println("Error al crear el archivo temporal:", err)
        return
    }
    defer archivoTemporal.Close()
    fmt.Println("Se creó el archivo temporal:", archivoTemporal.Name())
}
```

Esta forma da más control sobre el archivo temporal, permitiéndonos especificar su ubicación y permisos.

## Ver también

- [Documentación de la función "ioutil.TempFile()"](https://golang.org/pkg/io/ioutil/#TempFile)
- [Documentación de la función "ioutil.TempDir()"](https://golang.org/pkg/io/ioutil/#TempDir)
- [Documentación de la función "os.Create()"](https://golang.org/pkg/os/#Create)