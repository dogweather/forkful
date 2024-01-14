---
title:                "Ruby: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con una larga lista de datos que necesitas procesar en tu programa de Ruby? ¿Tienes problemas para agregar esa lista a tu código de manera organizada y legible? Ahí es donde YAML entra en juego. YAML es un formato de serialización de datos que se utiliza para almacenar y transferir datos de manera estructurada y fácilmente legible para humanos. Con YAML, puedes separar fácilmente tus datos en secciones y organizarlos de manera clara, lo que facilita su uso en tus programas.

## Cómo hacerlo

Primero, necesitas instalar la gema de YAML en tu proyecto de Ruby. Puedes hacerlo agregando `require 'yaml'` al principio de tu archivo y luego ejecutando `bundle install` en tu terminal. Una vez que la gema esté instalada, puedes comenzar a trabajar con YAML en tu código.

Para crear un YAML, utiliza `YAML.dump`, pasando un objeto de Ruby como argumento. Por ejemplo, si tienes una lista de nombres en un array, puedes crear un YAML con estos nombres de la siguiente manera:

```Ruby
nombres = ["María", "Juan", "Sofía", "Pedro"]
yml = YAML.dump(nombres)
```

Esto creará una cadena de texto que contiene los nombres en formato YAML:

```YAML
- María
- Juan
- Sofía
- Pedro
```

Si quieres guardar este YAML en un archivo, simplemente utiliza `File.write`:

```Ruby
File.write("nombres.yml", yml)
```

Para leer un YAML en tu programa Ruby, puedes utilizar `YAML.load` y pasarle la ruta del archivo como argumento:

```Ruby
nombres = YAML.load("nombres.yml")
```

Esto convertirá el YAML en un objeto de Ruby (en este caso, un array) y puedes trabajar con él en tu programa.

## Profundizando

Además de poder crear y leer YAML, también puedes editarlos en tiempo de ejecución. Por ejemplo, puedes agregar más datos a un YAML existente utilizando `YAML.load`, actualizando el objeto de Ruby y luego volviendo a guardar el YAML utilizando `YAML.dump`.

Además, YAML admite diferentes tipos de datos, como strings, arrays, hashes y también puedes agregar tus propios tipos de datos personalizados. Puedes explorar más sobre el formato YAML y sus opciones de configuración en la documentación oficial: https://yaml.org/spec/1.2/spec.html.

## Ver también

- Documentación oficial de YAML: https://yaml.org/spec/1.2/spec.html.
- Tutorial de YAML en RubyGems: https://rubygems.org/gems/yaml.
- Ejemplos de YAML en proyectos de código abierto: https://github.com/search?q=yaml.

¡Espero que este artículo te haya ayudado a comprender mejor cómo trabajar con YAML en tus proyectos de Ruby! ¡Inténtalo y descubre lo fácil que es organizar tus datos con este formato de serialización!