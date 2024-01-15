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

## Por qué

Si estás interesado en programas de computación, es probable que hayas oído hablar de YAML. Se trata de un formato de serialización de datos muy popular en el mundo de la programación. Pero ¿por qué es tan importante trabajar con YAML?

YAML es una forma organizada y sencilla de almacenar y compartir datos estructurados. Se puede leer fácilmente tanto por humanos como por máquinas, lo que lo hace ideal para configuraciones y archivos de datos en aplicaciones web.

## Cómo hacerlo

Para trabajar con YAML en Ruby, necesitarás instalar la gema "yaml". Puedes hacerlo ejecutando el siguiente comando en tu terminal:

```
gem install yaml
```

Una vez instalada la gema, podrás utilizar el módulo YAML en tu código de Ruby. Aquí tienes un ejemplo de cómo crear un archivo YAML y leerlo:

```
require 'yaml'

# Crear y escribir un archivo YAML
File.open('datos.yaml', 'w') do |file|
  file.write({
    nombre: 'María',
    edad: 25,
    intereses: ['programación', 'viajar', 'leer']
    }.to_yaml)
end

# Leer el archivo YAML creado
datos = YAML.load(File.read('datos.yaml'))

# Acceder a los datos específicos
puts datos[:nombre] # María
puts datos[:edad] # 25
puts datos[:intereses] # ['programación', 'viajar', 'leer']
```

También puedes utilizar YAML para leer y escribir archivos de configuración en tus proyectos de Ruby. Por ejemplo, en una aplicación web podrías tener un archivo "config.yaml" con la configuración de tu base de datos o de otros servicios externos.

## Profundizando

Para profundizar en el uso de YAML en Ruby, puedes consultar la documentación oficial de Ruby sobre YAML y sus diferentes opciones y métodos. También puedes explorar otras gemas de YAML que pueden proporcionar funcionalidades adicionales como la validación de archivos YAML o la conversión de YAML a otros formatos.

Otra forma de aprender más es explorando proyectos existentes que utilizan YAML en su código. De esta manera, podrás ver cómo otros programadores han utilizado YAML en sus aplicaciones y aprender de sus prácticas.

## Ver también

- [Documentación oficial de Ruby sobre YAML](https://ruby-doc.org/stdlib-2.7.0/libdoc/yaml/rdoc/YAML.html)
- [Gema de validación de archivos YAML](https://rubygems.org/gems/yaml-lint)
- [Gema de conversión de YAML a JSON](https://rubygems.org/gems/yaml2json)