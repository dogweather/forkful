---
title:                "Go: Leyendo un archivo de texto"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto

Hay muchas razones por las cuales podrías querer leer un archivo de texto en tu programa de Go. Tal vez necesites procesar información de un archivo de configuración, leer datos de entrada del usuario o analizar un archivo de registro. Independientemente de la razón, aprender cómo leer archivos de texto en Go es una habilidad esencial para cualquier programador.

## Cómo hacerlo

Para leer un archivo de texto en Go, primero debes abrir el archivo utilizando la función ```os.Open()``` y pasarle la ruta del archivo como argumento. Luego, debes crear un búfer de lectura utilizando la función ```bufio.NewReader()``` y pasarle como argumento el archivo abierto. Finalmente, puedes usar el bucle ```for``` y el método ```ReadString()``` para leer cada línea del archivo hasta que se llegue al final.

Un ejemplo de código podría ser el siguiente:

```Go
archivo, err := os.Open("archivo.txt") // Abre el archivo
if err != nil {
  fmt.Println("Error al abrir el archivo:", err)
}

lector := bufio.NewReader(archivo) // Crea un búfer de lectura
for {
  cadena, err := lector.ReadString('\n') // Lee una línea del archivo
  if err != nil {
    if err == io.EOF { // Comprueba si se llegó al final del archivo
      break
    }
    fmt.Println("Error al leer el archivo:", err)
  }
  fmt.Println(cadena) // Imprime la línea leída
}
```

Este código abrirá el archivo llamado "archivo.txt" y leerá cada línea del mismo hasta que se llegue al final. Puedes ver el resultado impreso en la consola.

## Profundizando

Además de leer líneas de un archivo de texto, Go ofrece otras funcionalidades para trabajar con archivos, como la función ```ioutil.ReadFile()``` que lee todo el contenido de un archivo en un solo paso, o el método ```Scanner.Scan()``` que te permite escanear palabras, números o incluso tokens de un archivo.

También puedes utilizar saltos de línea especiales, como ```\r\n``` o ```\r```, dependiendo del sistema operativo en el que estés trabajando. Además, debes tener en cuenta que es importante cerrar el archivo una vez que hayas terminado de leerlo utilizando el método ```Close()```.

## Ver también

- [Documentation for os package in Go](https://golang.org/pkg/os/)
- [Go by Example: Reading Files](https://gobyexample.com/reading-files)
- [Tutorial: Reading and Writing Files in Go](https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-go)