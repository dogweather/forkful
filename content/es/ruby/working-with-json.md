---
title:                "Trabajando con json"
html_title:           "Ruby: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON?

Trabajar con JSON, o JavaScript Object Notation, es esencial para cualquier desarrollador de software en la actualidad. JSON es un formato de intercambio de datos ligero y fácil de entender, lo que lo hace ideal para la transferencia de datos entre aplicaciones y sistemas. Con la popularidad de las API y las aplicaciones web, es casi imposible evitar trabajar con JSON.

## Cómo hacerlo

Para trabajar con JSON en Ruby, necesitarás instalar la gema `json` si aún no está instalada en tu sistema. Puedes hacerlo ejecutando el siguiente comando en tu terminal:

```ruby
gem install json
```

Una vez que tengas la gema `json` instalada, puedes comenzar a utilizarla en tus proyectos de Ruby. A continuación, se muestra un ejemplo simple de cómo convertir un objeto Ruby en un formato JSON:

```ruby
require 'json'

# Creamos un hash en Ruby
my_hash = { name: "Juan", age: 25, hobbies: ["correr", "leer", "viajar"] }

# Convertimos el hash a formato JSON
json_string = JSON.generate(my_hash)

puts json_string
# => {"name":"Juan","age":25,"hobbies":["correr","leer","viajar"]}
```

Como se puede ver en el ejemplo, utilizamos el método `JSON.generate` para convertir nuestro hash a formato JSON. Luego, podemos imprimir la cadena JSON en la consola.

También podemos realizar el proceso inverso, es decir, convertir una cadena JSON en un objeto Ruby. A continuación se muestra un ejemplo de cómo hacerlo:

```ruby
require 'json'

# Cadena JSON
json_string = '{"name":"Maria","age":30,"hobbies":["pintar","bailar","cocinar"]}'

# Convertimos la cadena a objeto Ruby
my_hash = JSON.parse(json_string)

puts my_hash[:name] # imprime "Maria"
puts my_hash[:hobbies] # imprime ["pintar","bailar","cocinar"]
```

En este ejemplo, utilizamos el método `JSON.parse` para convertir la cadena JSON en un objeto Ruby. Luego, podemos acceder a los valores del objeto como lo haríamos con cualquier otro objeto Ruby.

## Profundizando en JSON

Una de las mejores cosas de trabajar con JSON en Ruby es que la gema `json` incluye un conjunto de métodos útiles que facilitan el procesamiento y manipulación de datos JSON. Por ejemplo, si queremos agregar un nuevo elemento a un arreglo JSON existente, podemos utilizar el método `JSON.generate` junto con el método `<<`:

```ruby
require 'json'

# Cadena JSON inicial
json_string = '{"fruits":["manzana","plátano","mango"]}'

# Convertimos la cadena a objeto Ruby
my_hash = JSON.parse(json_string)

# Agregamos una nueva fruta al arreglo
my_hash["fruits"] << "uva"

# Convertimos el objeto Ruby a formato JSON
updated_json = JSON.generate(my_hash)

puts updated_json 
# => {"fruits":["manzana","plátano","mango","uva"]}
```

Además, la gema `json` también tiene un método llamado `pretty_generate` que permite formatear la salida JSON de una manera más legible para los humanos. Esto es especialmente útil al trabajar con cadenas JSON largas o complejas.

## Ver también

- [Documentación oficial de la gema JSON en Ruby](https://rubygems.org/gems/json)
- [Guía rápida de JSON en Ruby por RubyGuides](https://www.rubyguides.com/2018/10/json-ruby/)
- [Tutorial de JSON en Ruby en DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-ruby)