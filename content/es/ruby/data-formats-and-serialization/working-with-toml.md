---
title:                "Trabajando con TOML"
aliases: - /es/ruby/working-with-toml.md
date:                  2024-01-26T04:25:32.271561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

TOML es un formato de archivo de configuración que es fácil de leer debido a su clara semántica. Los programadores usan TOML para gestionar configuraciones de aplicaciones y serialización de datos sin el peso de XML o las peculiaridades de YAML.

## Cómo hacerlo:

Primero, instala la gema `toml-rb`. Es una opción popular para el análisis de TOML en Ruby.

```Ruby
gem install toml-rb
```

A continuación, leyendo un archivo TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Un ejemplo de salida podría ser:

```
Mi Aplicación Asombrosa
```

Escribiendo en un archivo TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'Mi Aplicación Asombrosa',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Comprueba `config.toml` y verás tus ajustes, almacenados de forma ordenada.

## Profundización

TOML, que significa Lenguaje Mínimo y Obvio de Tom, fue creado por Tom Preston-Werner, el co-fundador de GitHub, alrededor de 2013. Su objetivo principal es ser un formato sencillo fácil de analizar en estructuras de datos. Mientras que JSON es excelente para APIs y YAML es flexible, el nicho de TOML es su énfasis en ser amigable para los humanos. A diferencia de YAML, que puede ser quisquilloso con la indentación, TOML apunta a una estructura más parecida a INI que muchos encuentran más simple y con menos propensión a errores.

Alternativas como JSON, YAML o XML tienen cada una sus propias fortalezas, pero TOML prospera en escenarios donde una configuración debería ser fácilmente mantenida por humanos y programas por igual. No solo es más simple, sino que impone un formato estricto y legible.

En el lado técnico, para analizar contenido TOML con Ruby, aprovechamos gemas como `toml-rb`. Esta gema aprovecha la naturaleza dinámica de Ruby, convirtiendo datos TOML en hashes nativos de Ruby, arrays y otras estructuras de datos básicas. Esta conversión significa que los desarrolladores pueden trabajar con datos TOML usando semántica y métodos familiares de Ruby.

## Ver también

- Proyecto y especificación de TOML: https://toml.io/en/
- La gema `toml-rb`: https://github.com/emancu/toml-rb
- Comparando TOML, YAML y JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
