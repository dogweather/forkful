---
title:                "Leyendo un archivo de texto."
html_title:           "Go: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado leer un archivo de texto en tus programas Go? Si es así, ¡has venido al lugar correcto! En este artículo, aprenderás cómo leer un archivo de texto de manera fácil y eficiente utilizando el lenguaje de programación Go.

## Cómo

Para leer un archivo de texto en Go, primero necesitas abrir el archivo utilizando la función `os.Open()` y luego leer su contenido utilizando la función `bufio.NewScanner()`.

```
file, err := os.Open("archivo.txt") // abre el archivo
if err != nil {
    log.Fatal(err)
}

scanner := bufio.NewScanner(file) // crea un escáner para leer el archivo

for scanner.Scan() { // itera sobre cada línea del archivo
    fmt.Println(scanner.Text()) // imprime la línea actual del archivo
}

if err := scanner.Err(); err != nil {
    log.Fatal(err)
}
```

El código anterior abre el archivo "archivo.txt" y lo lee línea por línea hasta que llega al final del archivo. Cada línea se imprime en la consola utilizando la función `fmt.Println()`.

## Profundizando

Ahora que sabes cómo leer un archivo de texto en Go, es importante entender cómo funciona este proceso en detalle. Cuando utilizamos la función `os.Open()` para abrir un archivo, el sistema operativo crea un *puntero* al archivo, lo que nos permite acceder a su contenido.

Luego, al utilizar la función `bufio.NewScanner()`, creamos un *escáner* que nos permite leer el contenido del archivo línea por línea. El escáner también se encarga de manejar cualquier tipo de error que pueda surgir durante la lectura del archivo.

Una vez que hayamos terminado de leer el archivo, es importante cerrarlo utilizando la función `file.Close()`. Esto liberará cualquier recurso utilizado durante la lectura del archivo y evitará posibles errores en nuestro programa.

## Ver también

Si quieres saber más sobre cómo trabajar con archivos en Go, aquí tienes algunos recursos adicionales:

- [Documentación oficial de Go sobre archivos](https://golang.org/pkg/os/#File)
- [Tutorial sobre cómo leer y escribir archivos en Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go-es)
- [Ejemplos prácticos de cómo trabajar con archivos en Go](https://www.calhoun.io/working-with-files-in-go/)