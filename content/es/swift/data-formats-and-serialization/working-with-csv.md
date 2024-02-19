---
aliases:
- /es/swift/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:22.743644-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar datos estructurados a partir de archivos de texto donde cada l\xEDnea\u2026"
lastmod: 2024-02-18 23:09:10.382688
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar datos estructurados a partir de archivos de texto donde cada l\xEDnea\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con archivos CSV (Valores Separados por Comas) implica analizar y generar datos estructurados a partir de archivos de texto donde cada línea representa un registro y cada registro consiste en campos separados por comas. Los programadores a menudo participan en esta actividad para importar, exportar y manipular datos tabulares fácilmente utilizando un formato que es ampliamente compatible en diferentes plataformas y lenguajes de programación, debido a su simplicidad y formato legible por humanos.

## Cómo hacerlo:

En Swift, no hay soporte nativo para analizar archivos CSV directamente, pero puedes manejar datos CSV utilizando los métodos de `String` para dividir los contenidos, o aprovechando bibliotecas de terceros como SwiftCSV para un enfoque más simplificado. Aquí están ambos métodos:

### Análisis Manual sin Bibliotecas Externas
```swift
// Considera una simple cadena CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Ángeles
"""

// Divide la cadena CSV en líneas
let rows = csvString.components(separatedBy: "\n")

// Extrae las claves de la primera fila
let keys = rows.first?.components(separatedBy: ",")

// Itera sobre las filas empezando desde la segunda
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Salida de Ejemplo
print(result)
// Imprime: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Ángeles", "age": "34", "name": "Jane Smith"}]
```
Este enfoque es directo pero carece de robustez, especialmente con archivos CSV que contienen casos especiales como comas en valores, saltos de línea dentro de campos, etc.

### Usando la Biblioteca SwiftCSV
Primero, agrega SwiftCSV a tu proyecto incluyéndolo en tus dependencias de `Package.swift`:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Luego, impórtalo y úsalo de la siguiente manera:
```swift
import SwiftCSV

// Asume que `csvString` está definido como arriba

// Crea un objeto CSV
if let csv = try? CSV(string: csvString) {
    // Accede a las filas como diccionarios
    let rows = csv.namedRows
    
    // Salida de Ejemplo
    print(rows)
    // Imprime: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Ángeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV simplifica el análisis al lidiar automáticamente con matices como comas encapsuladas, saltos de línea en campos y codificación de caracteres. Sin embargo, recuerda manejar posibles errores en aplicaciones del mundo real, especialmente al tratar con fuentes de datos externas.
