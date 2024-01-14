---
title:    "Go: Escribiendo un archivo de texto"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto
Escribir archivos de texto es una tarea común en la programación. Puede ser útil para almacenar datos, configuraciones, mensajes de error o cualquier otra información que necesite ser guardada y recuperada en un formato legible por humanos. Además, escribir un archivo de texto también puede ser útil para crear registros o archivos de registro.

## Cómo hacerlo
Para escribir un archivo de texto en Go, primero necesitamos importar el paquete "os" que nos permite interactuar con el sistema operativo. Luego, utilizamos la función `Create()` del paquete "os" para crear o abrir un archivo en modo escritura. Luego, podemos utilizar un objeto de tipo `File` para escribir en el archivo utilizando el método `WriteString()` o `Write()`.

```Go
import (
    "fmt"
    "os"
)

func main() {
    archivo, err := os.Create("mi_archivo.txt")
    if err != nil {
        fmt.Println("Error al crear el archivo. ", err)
        return
    }
    defer archivo.Close()

    // Escribir en archivo usando WriteString()
    archivo.WriteString("Este es un ejemplo de texto que será escrito en el archivo.\n")

    // Escribir en archivo usando Write()
    mensaje := []byte("Este es otro mensaje escrito en el archivo.")
    _, err = archivo.Write(mensaje)
    if err != nil {
        fmt.Println("Error al escribir en el archivo. ", err)
        return
    }
}
```

El resultado sería un archivo de texto llamado "mi_archivo.txt" con el siguiente contenido:

```
Este es un ejemplo de texto que será escrito en el archivo.
Este es otro mensaje escrito en el archivo.
```

## Profundizando
Hay algunas cosas a tener en cuenta al escribir archivos de texto en Go:

- Si el archivo especificado en el método `Create()` ya existe, será truncado, es decir, su contenido anterior será eliminado.
- Podemos utilizar el método `Write()` para escribir bytes en el archivo, mientras que `WriteString()` solo acepta cadenas.
- Debemos cerrar el archivo utilizando el método `Close()` al finalizar la escritura para asegurarnos de que todos los datos hayan sido escritos correctamente en el archivo.
- Siempre es buena práctica verificar los errores al interactuar con el sistema operativo utilizando las funciones del paquete "os".

## Ver también
- [Documentación oficial de Go sobre el paquete "os"](https://golang.org/pkg/os/)
- [Tutorial de Go en español](https://www.golang-book.com/es/)
- [Ejemplo de escritura de archivos en Go](https://gobyexample.com/writing-files)