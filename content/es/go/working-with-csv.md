---
title:                "Go: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV en Go?

CSV (Comma-Separated Values) es un formato de archivo utilizado para almacenar datos de forma tabular, utilizando comas como separadores entre cada valor. Este formato es ampliamente utilizado en aplicaciones que manejan grandes cantidades de datos, como hojas de cálculo o bases de datos. En Go, trabajar con archivos CSV puede ser muy útil para manipular y procesar datos de forma eficiente. En esta entrada de blog, vamos a explorar por qué deberías considerar trabajar con CSV en tus proyectos de Go.

## ¿Cómo trabajar con CSV en Go?

Para trabajar con CSV en Go, necesitaremos utilizar el paquete "encoding/csv" que viene incluido en la biblioteca estándar. Empezaremos por importar este paquete en nuestro código:

```Go
import "encoding/csv"
```

Luego, podemos utilizar la función `NewReader` para crear un objeto que nos permitirá leer los datos de un archivo CSV:

```Go
file, err := os.Open("datos.csv")
if err != nil {
    log.Fatal("Error al abrir el archivo:", err)
}

reader := csv.NewReader(file)
```

Una vez creado el objeto `reader`, podemos utilizar el método `Read` para leer los datos del archivo línea por línea:

```Go
for {
    record, err := reader.Read()
    if err == io.EOF {
        break
    } else if err != nil {
        log.Fatal("Error al leer el archivo:", err)
    }

    // Procesar cada línea de datos aquí
}
```

El método `Read` nos devuelve un slice con los valores de cada celda de la línea actual. Podemos acceder a cada celda utilizando su índice en el slice, por ejemplo `record[0]` para acceder a la primera celda.

Ahora, podemos imprimir los datos de cada línea en la consola para ver cómo se están leyendo:

```Go
fmt.Println("Nombre:", record[0])
fmt.Println("Apellido:", record[1])
fmt.Println("Edad:", record[2])
```

La salida en la consola será algo como esto:

```
Nombre: Juan
Apellido: Pérez
Edad: 25
```

Para escribir datos en un archivo CSV, podemos utilizar el método `Write` y pasarle un slice con los valores que queremos escribir en cada celda de la línea:

```Go
writer := csv.NewWriter(file)

nombre := "María"
apellido := "Gómez"
edad := "30"

datos := []string{nombre, apellido, edad}

writer.Write(datos)
writer.Flush()
```

Al utilizar `flush`, nos aseguramos de que los datos sean escritos en el archivo inmediatamente. De lo contrario, los cambios no se guardarán hasta que el programa termine de ejecutarse.

## Profundizando en el trabajo con CSV en Go

El paquete `encoding/csv` también nos ofrece otras funciones útiles para trabajar con CSV, como `ReadAll` para leer todos los datos del archivo y guardarlos en un slice, y `WriteAll` para escribir una matriz de datos en el archivo.

Además, es importante tener en cuenta que podemos especificar diferentes separadores en lugar de la coma predeterminada, utilizando `reader.Comma` y `writer.Comma`.

También es posible manejar errores al leer o escribir en un archivo CSV utilizando el objeto `Error` devuelto por los métodos de lectura y escritura de `csv`.

Para obtener más información sobre cómo trabajar con CSV en Go, puedes consultar la documentación oficial del paquete `encoding/csv`.

## Ver también

- Documentación oficial del paquete `encoding/csv`: https://golang.org/pkg/encoding/csv
- Ejemplos de código para trabajar con CSV en Go: https://github.com/golang/go/wiki/CSV

¡Esperamos que esta entrada de blog te haya sido útil en tu aprendizaje de cómo trabajar con CSV en Go! ¡No dudes en dejar comentarios y compartir con tus amigos programadores! ¡Hasta la próxima!