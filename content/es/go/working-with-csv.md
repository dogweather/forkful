---
title:                "Trabajando con csv"
html_title:           "Go: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV en Go

CSV (Comma Separated Values) es un formato de archivo ampliamente utilizado para almacenar y transferir datos tabulares. Trabajar con archivos CSV en Go puede ser beneficioso para aquellos que necesitan manipular grandes cantidades de datos de forma eficiente, ya que es un lenguaje de programación de alto rendimiento diseñado específicamente para el procesamiento de datos.

## Cómo hacerlo

Para trabajar con archivos CSV en Go, primero necesitamos importar el paquete "csv":

```Go
import "encoding/csv"
```

Una vez importado, podemos utilizar la función "ReadFile" para leer un archivo CSV y almacenar sus datos en una matriz de acuerdo a su estructura:

```Go
file, err := os.Open("datos.csv") //abre el archivo CSV
if err != nil { //si hay un error en la apertura del archivo
    log.Fatal(err) //el programa se detiene y muestra el error
}
defer file.Close() //cierra el archivo al finalizar el programa

reader := csv.NewReader(file) //crea un lector para el archivo CSV
records, err := reader.ReadAll() //almacena los datos del archivo en una matriz
if err != nil { //si hay un error al leer el archivo
    log.Fatal(err) //el programa se detiene y muestra el error
}
```

Luego, podemos acceder a los datos de la matriz y realizar diferentes operaciones, como imprimirlos en pantalla o guardarlos en una base de datos. Por ejemplo, para imprimir los datos del archivo CSV en pantalla, podemos utilizar un bucle for:

```Go
for _, row := range records { //itera sobre cada fila de la matriz
    fmt.Println(row) //imprime la fila en pantalla
}
```

El resultado de este código sería algo similar a esto:

```
["nombre", "edad", "país"]
["Juan", "25", "Argentina"]
["María", "30", "España"]
["Pedro", "28", "México"]
```

## Profundizando en el tema

El paquete "csv" en Go proporciona muchas más funciones y opciones para trabajar con archivos CSV. Por ejemplo, se pueden especificar delimitadores y caracteres de cita para la lectura y escritura de archivos, y también hay opciones para manejar registros con diferentes tipos de datos.

Para obtener más información sobre las funciones disponibles y cómo utilizarlas, se recomienda consultar la documentación oficial del paquete "csv" de Go: https://golang.org/pkg/encoding/csv/

## Ver también

- Para obtener más información sobre Go (versión actual), visita la página oficial: https://golang.org/
- Para aprender más sobre la estructura de un archivo CSV y su uso, puedes consultar este artículo: https://www.tecnologiapyme.com/como-funcionan-los-archivos-csv/