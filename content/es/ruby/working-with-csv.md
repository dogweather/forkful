---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Trabajar con CSV significa manipular datos almacenados en un formato de texto sencillo, conocido como 'Valores Separados por Comas' (CSV). Los programadores lo usan por su simplicidad y amplia aceptación como medio de intercambio de datos entre sistemas y aplicaciones.

## How to:
Ruby facilita el trabajo con archivos CSV con su biblioteca estándar. Aquí hay ejemplos básicos:

```Ruby
require 'csv'

# Leer CSV desde un archivo
CSV.foreach("datos.csv") do |fila|
  puts fila.inspect
end

# Crear un nuevo archivo CSV
CSV.open("nuevo_datos.csv", "wb") do |csv|
  csv << ["nombre", "edad", "ciudad"]
  csv << ["Alejandro", 25, "Madrid"]
  csv << ["Carmen", 30, "Barcelona"]
end
```

Salida al leer `datos.csv`:
```
["nombre", "edad", "ciudad"]
["Alejandro", "25", "Madrid"]
["Carmen", "30", "Barcelona"]
```

## Deep Dive
CSV es un formato que ha existido desde antes que las hojas de cálculo fueran populares. Alternativas modernas incluyen JSON y XML, pero CSV sigue siendo relevante por su simplicidad y la baja sobrecarga en la transmisión de datos. Ruby implementa manejo de CSV en su biblioteca estándar (`csv`), permitiendo conversión fácil entre CSV y arrays/objetos.

## See Also
- [Ruby CSV Library Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/csv/rdoc/CSV.html)
- [RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://datatracker.ietf.org/doc/html/rfc4180)
- [Tutorial de Ruby](https://www.ruby-lang.org/es/documentation/quickstart/)
