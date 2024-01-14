---
title:    "Go: Comprobando si existe un directorio."
keywords: ["Go"]
---

{{< edit_this_page >}}

# Por qué

¿Alguna vez te has encontrado en la situación de necesitar saber si un directorio existe en tu sistema? Ya sea para organizar tus archivos o para validar una ruta de acceso en tu código, verificar la existencia de un directorio puede ser una tarea importante en el desarrollo de aplicaciones en Go.

# Cómo hacerlo

En Go, podemos utilizar la función `os.Stat()` para verificar la existencia de un directorio. Esta función devuelve un error si el directorio no existe o una estructura `FileInfo` si lo hace.

```Go
if _, err := os.Stat("directorio/ejemplo"); err == nil {
    fmt.Println("El directorio existe")
} else {
    fmt.Println("El directorio no existe")
}
```

Si deseas verificar la existencia de un directorio de forma recursiva (es decir, en caso de que tenga subdirectorios), puedes utilizar la función `os.Stat()` en conjunto con `filepath.Walk()`, que recorre de forma recursiva una ruta de acceso y ejecuta una función en cada archivo o directorio que encuentre.

```Go
err := filepath.Walk("directorio/ejemplo", func(path string, info os.FileInfo, err error) error {
    if err != nil {
        return err
    }
    if info.IsDir() {
        fmt.Println("El directorio", path, "existe")
    }
    return nil
})
if err != nil {
    fmt.Println(err)
}
```

# Profundizando

La función `os.Stat()` también es útil para obtener información sobre un directorio, como su tamaño o la fecha de su última modificación. Además, podemos utilizar `os.IsNotExist()` para verificar si un error específico corresponde a un directorio que no existe.

# Ver también

- [Documentación oficial de os.Stat() en Go](https://pkg.go.dev/os#Stat)
- [Documentación oficial de filepath.Walk() en Go](https://pkg.go.dev/path/filepath#Walk)
- [Tutorial sobre manejo de errores en Go](https://www.digitalocean.com/community/tutorials/how-to-handle-errors-in-go-es)