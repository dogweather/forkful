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

## Por qué

Si estás interesado en el procesamiento de datos y análisis, trabajar con archivos CSV es una habilidad esencial para poder manipular y utilizar datos de manera eficiente. Además, muchas aplicaciones y proyectos utilizan archivos CSV como formato de almacenamiento de datos, por lo que conocer cómo trabajar con ellos es valioso para cualquier desarrollador.

## Cómo hacerlo

Para trabajar con archivos CSV en Ruby, necesitamos utilizar la clase `CSV` que es parte de la librería estándar. Primero, debemos requerir esta clase en nuestro código:

```Ruby
require 'csv'
```

Una vez que tenemos la clase `CSV` disponible, podemos utilizar sus métodos para leer, escribir y manipular archivos CSV.

### Leer archivos

Para leer un archivo CSV, podemos usar el método `read` de la clase `CSV`, pasando como argumento la ruta al archivo CSV. Este método devolverá un arreglo con los datos del archivo.

```Ruby
data = CSV.read("archivo.csv")
```

Si queremos especificar un separador diferente del valor por defecto (coma), podemos pasar un segundo argumento con el separador como una cadena.

```Ruby
data = CSV.read("archivo.csv", ";")
```

Si nuestro archivo CSV contiene encabezados, podemos usar el método `read_headers` en lugar de `read`, que tomará la primera línea del archivo como los nombres de las columnas y devolverá un objeto `CSV::Table` que nos permite acceder a los datos por nombre de columna.

```Ruby
tabla = CSV.read_headers("archivo.csv")
# Acceder a la fila 2, columna "nombre"
puts tabla[1]["nombre"]
```

### Escribir archivos

Para escribir en un archivo CSV, podemos usar el método `open` de la clase `CSV`, pasando como argumentos la ruta al archivo y el modo de escritura. Luego, podemos usar el método `<<` para agregar datos.

```Ruby
CSV.open("nuevo_archivo.csv", "w") do |csv|
  csv << ["Nombre", "Apellido"]
  csv << ["Juan", "Pérez"]
  csv << ["María", "González"]
end
```

### Manipulación de datos

La clase `CSV` también nos proporciona otros métodos útiles para manipular datos en archivos CSV, como `parse`, que nos permite convertir una cadena con formato CSV a un arreglo o `CSV.table`, que nos devuelve una tabla similar al método `read_headers` que mencionamos anteriormente.

## Profundizando

Si quieres profundizar en el trabajo con archivos CSV en Ruby, puedes consultar la documentación oficial de la clase `CSV` y la librería `csv` para obtener más información y conocer todas las opciones disponibles.

## Ver también

- [Documentación oficial de la clase CSV en Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html)
- [Página de la librería csv en RubyGems](https://rubygems.org/gems/csv)