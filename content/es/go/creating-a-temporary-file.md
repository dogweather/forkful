---
title:    "Go: Creando un archivo temporal"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Go

Crear un archivo temporal puede ser necesario en diferentes situaciones de programación en Go. Por ejemplo, si necesitas guardar datos de manera temporal en la memoria para luego ser eliminados, o para hacer pruebas con datos que no quieres guardar permanentemente.

## Cómo crear un archivo temporal en Go

Para crear un archivo temporal en Go, se puede utilizar la función `TempFile` del paquete `io/ioutil`. Esta función toma dos argumentos: la ruta donde quieres crear el archivo temporal y un prefijo para el nombre del archivo. A continuación, se muestra un ejemplo de código utilizando esta función:

```Go
file, err := ioutil.TempFile("ruta", "prefijo")
if err != nil {
  log.Fatal(err)
}
defer os.Remove(file.Name())

fmt.Println("Se ha creado un archivo temporal en ", file.Name())
```

El código anterior creará un archivo temporal en la ruta especificada con un nombre que comience con el prefijo especificado. También se utiliza la función `defer` para asegurarse de que el archivo se elimine automáticamente después de finalizar el proceso.

## Profundizando en la creación de archivos temporales

Al crear un archivo temporal en Go, también puedes especificar una extensión para el archivo utilizando la función `TempFile`. Por ejemplo:

```Go
file, err := ioutil.TempFile("ruta", "prefijo.*.txt")
if err != nil {
  log.Fatal(err)
}
defer os.Remove(file.Name())

fmt.Println("Se ha creado un archivo temporal en ", file.Name())
```

Además, puedes utilizar la función `TempDir` del paquete `ioutil` para crear un directorio temporal en lugar de un archivo. Esta función también toma dos argumentos: una ruta donde quieres crear el directorio temporal y un prefijo para el nombre del directorio.

## Ver también

- [Documentación de la función TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Documentación de la función TempDir](https://golang.org/pkg/io/ioutil/#TempDir)