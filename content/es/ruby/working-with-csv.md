---
title:                "Trabajando con archivos csv"
html_title:           "Ruby: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## ¡Qué es y por qué hacemos esto?

Trabajar con CSV (Comma Separated Values) en Ruby es una forma de manejar datos tabulares utilizando un formato simple de texto plano que separa los valores con comas. Los programadores a menudo utilizan esto para importar y exportar datos de manera eficiente, especialmente cuando se trata de grandes cantidades de datos.

## ¡Cómo hacerlo!

```Ruby
require 'csv'
# Crear un archivo CSV
CSV.open("archivo.csv", "w") do |csv|
  csv << ["Nombre", "Edad", "Género"]
  csv << ["María", 25, "Femenino"]
  csv << ["Juan", 30, "Masculino"]
end

# Leer un archivo CSV existente
CSV.foreach("archivo.csv") do |row|
  puts row # cada fila es un array de valores separados por comas
end
```

Salida:
```
Nombre, Edad, Género
María, 25, Femenino
Juan, 30, Masculino
```

## Profundizando

El formato CSV fue creado en los años 70 como una forma de compartir datos entre diferentes aplicaciones. Aunque es ampliamente utilizado en la actualidad, también existen alternativas como JSON y XML para estructurar datos. En Ruby, el módulo CSV viene incluido en la biblioteca estándar, lo que significa que no es necesario instalar ninguna gema adicional para trabajar con este formato.

## Vea también

- [Documentación de Ruby sobre CSV](https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html)