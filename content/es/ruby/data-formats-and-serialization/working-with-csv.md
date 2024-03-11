---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:03.213949-07:00
description: "Trabajar con archivos CSV en Ruby ofrece un enfoque directo para manejar\
  \ datos tabulares. Los programadores a menudo se involucran en esta pr\xE1ctica\
  \ para\u2026"
lastmod: '2024-03-11T00:14:33.455752-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV en Ruby ofrece un enfoque directo para manejar\
  \ datos tabulares. Los programadores a menudo se involucran en esta pr\xE1ctica\
  \ para\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con archivos CSV en Ruby ofrece un enfoque directo para manejar datos tabulares. Los programadores a menudo se involucran en esta práctica para el análisis de datos, extracción, transformación y almacenamiento, convirtiéndolo en una habilidad crítica para tareas que involucran manipulación o análisis de datos.

## Cómo hacerlo:

Ruby incluye por defecto la biblioteca CSV, la cual simplifica la lectura y escritura de archivos CSV. Aquí te mostramos cómo puedes aprovechar esto para tareas comunes:

### Leer un archivo CSV
Para leer de un archivo CSV, primero requieres la biblioteca CSV. Luego, puedes iterar sobre las filas o leerlas en un arreglo.

```ruby
require 'csv'

# Leyendo cada fila como un arreglo
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# La salida para cada fila podría verse así: ["data1", "data2", "data3"]
```

### Escribir en un CSV
Escribir en un archivo CSV también es sencillo. Puedes añadir a un archivo existente o crear un nuevo archivo para escribir.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["encabezado1", "encabezado2", "encabezado3"]
  csv << ["valor1", "valor2", "valor3"]
end

# Esto crea o sobrescribe 'output.csv' con los encabezados y valores especificados.
```

### Analizar una cadena CSV
A veces necesitas analizar datos CSV directamente de una cadena. Así es cómo:

```ruby
require 'csv'

data = "nombre,edad,ciudad\nJohn Doe,29,Nueva York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['nombre']} - #{row['edad']} - #{row['ciudad']}"
end

# Salida esperada:
# John Doe - 29 - Nueva York
# Jane Doe - 31 - Chicago
```

### Usando SmarterCSV
Para tareas CSV más complejas, la gema `SmarterCSV` puede ser una herramienta valiosa. Primero, instala la gema:

```shell
gem install smarter_csv
```

Luego, puedes usarla para tratar con archivos grandes o realizar análisis y manipulación más sofisticados:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Esto leerá 'large_data.csv' y mostrará cada fila como un hash basado en los encabezados.
```

En resumen, la biblioteca CSV integrada en Ruby, junto con gemas de terceros como `SmarterCSV`, proporciona un soporte robusto para manejar datos CSV, permitiendo tareas eficientes de procesamiento y manipulación de datos.
