---
title:    "Go: Creando un archivo temporal"
keywords: ["Go"]
---

{{< edit_this_page >}}

##Por qué

La creación de archivos temporales es una práctica común en la programación, especialmente en entornos de prueba y desarrollo. Estos archivos proporcionan un espacio de trabajo temporal para almacenar datos, sin afectar los datos existentes en el sistema.

##Cómo

La creación de un archivo temporal en Go es muy sencilla gracias al paquete "io/ioutil". A continuación, se muestra un ejemplo de código que crea un archivo temporal y escribe datos en él:

```Go
archivoTemp, err := ioutil.TempFile("", "ejemplo")
if err != nil {
    fmt.Println(err)
}
defer archivoTemp.Close()
contenido := []byte("Ejemplo de datos")
_, err := archivoTemp.Write(contenido)
if err != nil {
    fmt.Println(err)
}
fmt.Println("Archivo temporal creado:", archivoTemp.Name())
```

El código anterior utiliza la función `TempFile` del paquete `ioutil` para crear un archivo temporal con el prefijo "ejemplo". Luego, el archivo se cierra y se escriben datos en él utilizando la función `Write`. Finalmente, se imprime el nombre del archivo temporal creado.

La salida del código anterior sería:

```
Archivo temporal creado: /tmp/ejemplo659395958
```

##Profundizando

Ahora que sabemos cómo crear un archivo temporal en Go, podemos explorar un poco más sobre este tema. Por ejemplo, es importante recordar que los archivos temporales deben eliminarse después de su uso, de lo contrario, pueden acumularse y ocupar espacio en el sistema.

Para eliminar un archivo temporal en Go, podemos utilizar la función `Remove` del paquete `os`. A continuación, se muestra un ejemplo de cómo eliminar un archivo temporal creado anteriormente:

```Go
err := os.Remove(archivoTemp.Name())
if err != nil {
    fmt.Println(err)
} else {
    fmt.Println("Archivo temporal eliminado:", archivoTemp.Name())
}
```

##Consulte también

- Documentación oficial de Go sobre el paquete `ioutil`: https://golang.org/pkg/io/ioutil/
- Tutorial de creación de archivos temporales en Go: https://tutorialedge.net/golang/create-temporary-file-go/
- Ejemplo de uso de archivos temporales en pruebas unitarias en Go: https://dev.to/moficodes/testing-files-in-go-ioutil-tempfile-example-1gnh