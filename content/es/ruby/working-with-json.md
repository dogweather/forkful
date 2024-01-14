---
title:                "Ruby: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON

Si estás interesado en desarrollar aplicaciones web o móviles, lo más probable es que hayas oído hablar acerca de JSON. JSON (JavaScript Object Notation) es un formato de texto sencillo utilizado para almacenar y transmitir datos estructurados. Ruby ofrece una amplia gama de herramientas y bibliotecas para trabajar con JSON, lo que lo convierte en una elección popular para muchos desarrolladores.

## Cómo trabajar con JSON en Ruby

Para comenzar a trabajar con JSON en Ruby, primero debemos instalar la biblioteca "JSON" utilizando el administrador de paquetes de Ruby, RubyGems.

```Ruby
gem install json
```

Una vez instalado, podemos comenzar a trabajar con JSON en nuestro código. A continuación, se muestra un ejemplo sencillo de cómo crear un objeto JSON y luego imprimirlo por pantalla:

```Ruby
require 'json'

# Creamos un objeto JSON
json_object = { "nombre": "Juan", "edad": 25, "ciudad": "Madrid" }

# Convertimos el objeto a formato JSON
json_string = JSON.generate(json_object)

# Imprimimos por pantalla
puts json_string
```

La salida de este código sería:

```
{"nombre":"Juan","edad":25,"ciudad":"Madrid"}
```

También podemos tomar un archivo JSON existente y convertirlo en un objeto Ruby para manipularlo. A continuación, se muestra un ejemplo de cómo leer un archivo JSON y acceder a sus valores:

```ruby
# Leemos el archivo JSON
json_file = File.read("datos.json")

# Convertimos el archivo a formato Ruby
data = JSON.parse(json_file)

# Accedemos a los valores del objeto
puts "El nombre es #{data["nombre"]}"
puts "La edad es #{data["edad"]}"
puts "La ciudad es #{data["ciudad"]}"
```

La salida de este código sería:

```
El nombre es Juan
La edad es 25
La ciudad es Madrid
```

## Profundizando en JSON

JSON es un formato de texto sencillo que sigue la misma sintaxis que los objetos y arrays de JavaScript. Se compone de pares de clave-valor donde los valores pueden ser cadenas de texto, números, booleanos, arrays o incluso otros objetos JSON anidados. Además, es un formato muy utilizado para intercambiar datos entre distintas aplicaciones debido a su simplicidad y compatibilidad con la mayoría de lenguajes de programación.

Al trabajar con JSON en Ruby, podemos utilizar la biblioteca "JSON" para convertir objetos Ruby en formato JSON y viceversa. También podemos utilizar el método `to_json` para convertir automáticamente un objeto Ruby en formato JSON.

Otra herramienta útil es "Active Support", parte del popular framework Ruby on Rails, que ofrece métodos para facilitar la manipulación de datos JSON, como `Hash.from_json` para convertir un objeto JSON en un objeto Ruby.

## Ver también

- [Documentación oficial de JSON en Ruby](https://ruby-doc.org/stdlib-2.4.1/libdoc/json/rdoc/JSON.html)
- [Introducción a JSON en Ruby](https://www.codecademy.com/learn/learn-ruby/modules/apis-ii/articles/json-in-ruby)
- [Especificación de JSON](https://tools.ietf.org/html/rfc8259)