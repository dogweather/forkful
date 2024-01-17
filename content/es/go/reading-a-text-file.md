---
title:                "Leyendo un archivo de texto"
html_title:           "Go: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer un archivo de texto es simplemente abrir un archivo que contiene texto y mostrar su contenido. Los programadores a menudo hacen esto para acceder a datos almacenados en archivos de texto, como configuraciones o información de usuarios.

## ¿Cómo hacerlo?

```Go
archivo, err := os.Open("miarchivo.txt")

if err != nil {
  fmt.Println("Error al abrir el archivo:", err)
}

scanner := bufio.NewScanner(archivo)

for scanner.Scan() {
  fmt.Println(scanner.Text())
}
```

### Salida de ejemplo

Si tenemos un archivo de texto con el siguiente contenido:

```
Hola
Mundo
```

La salida de nuestro programa sería:

```
Hola
Mundo
```

## Profundizando

Antes de que existieran las computadoras, los datos se almacenaban en archivos de texto legibles para los humanos. Hoy en día, este sigue siendo un método popular para almacenar datos simples y legibles para los programadores.

Si bien el código anterior se enfoca en leer un archivo línea por línea, también es posible leer un archivo de texto en su totalidad usando la función `ReadAll` de la biblioteca `ioutil`.

### Alternativas

Además de leer un archivo de texto, los programadores también pueden escribir en archivos de texto utilizando la función `WriteFile` de la biblioteca `ioutil`.

## Ver también

- [Go Documentation on File Handling](https://golang.org/pkg/os/)
- [File I/O in Go](https://medium.com/rungo/working-with-files-in-go-94a1cbe73c24)