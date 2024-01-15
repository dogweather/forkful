---
title:                "Trabajando con archivos csv"
html_title:           "Swift: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué
El formato CSV (valores separados por comas) es una forma popular de almacenar y compartir datos en una tabla simple. Al aprender a trabajar con archivos CSV en Swift, podrás manejar y analizar grandes conjuntos de datos de manera eficiente y efectiva.

## Cómo hacerlo
Trabajar con archivos CSV en Swift es muy sencillo gracias a la biblioteca incorporada `CSVKit`. Primero, importaremos `CSVKit` en nuestro proyecto:

```Swift
import CSVKit
```

Luego, podemos crear un lector CSV para cargar los datos de nuestro archivo. Supongamos que tenemos un archivo CSV llamado "datos.csv" con dos columnas, "Nombre" y "Edad", y queremos imprimir todos los nombres en nuestra consola. El código se vería así:

```Swift
let path = Bundle.main.path(forResource: "datos", ofType: "csv") // path al archivo CSV
let csv = try! CSV(url: path!) // crea un lector CSV
let nombres = csv.rows // obtiene una matriz de diccionarios con los datos
for persona in nombres { // recorre cada fila en la matriz
  print(persona["Nombre"]!) // imprime el nombre de cada persona
}
```

Y la salida será algo como:

```
Juan
María
Luis
Ana
```

## Profundizando
Aunque `CSVKit` nos facilita mucho el trabajo con archivos CSV, es importante destacar que existen diversas formas de trabajar con los datos cargados del archivo. Por ejemplo, podemos convertir la matriz de diccionarios a un objeto personalizado e incluso podemos realizar operaciones de filtrado o agregación de datos utilizando las funciones de la biblioteca `CSVSwift` que se basa en `CSVKit`.

## Ver también
- [Documentación oficial de CSVKit (en inglés)](https://cocoapods.org/pods/CSVKit)
- [Tutorial de trabajo con archivos CSV en Swift (en inglés)](https://medium.com/@aravindhomes/how-to-work-with-csv-files-in-swift-f5ecb958ace5)
- [Otras formas de trabajar con datos en Swift (en inglés)](https://www.hackingwithswift.com/read/32/2/from-swift-to-bash-using-command-line-arguments)