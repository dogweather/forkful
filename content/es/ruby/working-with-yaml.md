---
title:                "Trabajando con yaml"
html_title:           "Ruby: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué es YAML y por qué los programadores lo usan?
 YAML, YAML Ain't Markup Language, es un formato de serialización de datos de texto plano ampliamente utilizado en el mundo de la programación. Permite representar datos de forma legible tanto para humanos como para máquinas, lo que lo hace ideal para almacenar y compartir información en aplicaciones web, bases de datos o sistemas de configuración. Los programadores utilizan YAML principalmente por su simplicidad y flexibilidad, ya que es fácil de aprender y se puede adaptar a diferentes necesidades.

## Cómo hacerlo:
El formato YAML se basa en indentaciones y pares de claves y valores, similar a otros lenguajes como Python. A continuación se muestra un ejemplo de cómo se vería un archivo YAML:

```Ruby
# Ejemplo de YAML
nombre: Juan
apellido: Pérez
edad: 25
teléfono: 555-1234-5678 
```

Este código representa los datos de una persona en un formato estructurado y legible. Para acceder a estos datos en Ruby, se puede utilizar la gema YAML:

```Ruby
require "yaml"

datos = YAML.load_file("usuario.yaml") # Carga el archivo YAML en una variable

puts datos["nombre"] # Imprime "Juan"
puts datos["apellido"] # Imprime "Pérez"
puts datos["edad"] # Imprime 25
puts datos["teléfono"] # Imprime "555-1234-5678"
```

## Detalles técnicos:
YAML fue creado en 2001 y su primera versión fue publicada en 2002. A pesar de que se utiliza principalmente en entornos de programación web, también es usado en otros ámbitos como juegos de video o sistemas de configuración de servidores.

Existen otros formatos de serialización de datos como JSON o XML, pero YAML se destaca por su simplicidad y legibilidad. Al ser un lenguaje interpretado, no requiere un proceso de compilación y es compatible con la gran mayoría de los lenguajes de programación.

## Para saber más:
Si quieres aprender más sobre YAML, puedes consultar la documentación oficial en su página web (https://yaml.org/) o revisar la implementación de la gema YAML en GitHub (https://github.com/ai/psych). También hay numerosos tutoriales y ejemplos en línea que te pueden ayudar a familiarizarte con este formato de serialización de datos.