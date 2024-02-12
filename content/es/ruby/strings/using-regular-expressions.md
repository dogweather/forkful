---
title:                "Usando expresiones regulares"
aliases:
- /es/ruby/using-regular-expressions/
date:                  2024-02-03T19:17:57.154735-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expresiones regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares (regex) en Ruby son patrones utilizados para coincidir con combinaciones de caracteres en cadenas, lo que permite a los desarrolladores buscar, coincidir y manipular texto de manera eficiente. Los programadores utilizan regex para tareas tales como validación, análisis y manipulación de cadenas, lo que lo hace una herramienta indispensable para el procesamiento de texto.

## Cómo hacerlo:
### Coincidencia Básica
Para hacer coincidir una cadena con un patrón simple, puedes usar el método `match`. A continuación, estamos verificando si la palabra "Ruby" existe en una cadena dada.

```ruby
if /Ruby/.match("¡Hola, Ruby!")
  puts "¡Coincidencia encontrada!"
end
# Salida: ¡Coincidencia encontrada!
```

### Coincidencia de Patrones con Variables
Puedes interpolar variables en tu regex utilizando la sintaxis `#{}`, haciendo que tus patrones sean dinámicos.

```ruby
language = "Ruby"
if /#{language}/.match("Programar en Ruby es divertido.")
  puts "¡Hablando sobre Ruby!"
end
# Salida: ¡Hablando sobre Ruby!
```

### Usando Regex para Sustitución
El método `gsub` te permite reemplazar cada ocurrencia de un patrón con una cadena de reemplazo especificada.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Salida: barbarbar
```

### Capturando
Los paréntesis en un regex se utilizan para capturar partes de una coincidencia. El método `match` devuelve un objeto `MatchData`, que puedes utilizar para acceder a las capturas.

```ruby
match_data = /(\w+): (\d+)/.match("Edad: 30")
puts match_data[1] # Etiqueta capturada
puts match_data[2] # Valor capturado
# Salida:
# Edad
# 30
```

### Usando Bibliotecas de Terceros
Aunque la biblioteca estándar de Ruby es poderosa, a veces podrías necesitar funcionalidades más especializadas. Una gema popular para trabajar con regex es `Oniguruma`, que proporciona características de regex adicionales más allá del motor de regex integrado de Ruby.

Instálalo usando:
```bash
gem install oniguruma
```

Un ejemplo de uso podría verse así (asumiendo que has requerido `oniguruma` después de instalarlo):

```ruby
# Este es un ejemplo más avanzado y podría requerir configuración adicional
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("El número es 42.")
puts match_data[1]
# Salida: 42
```

Recuerda, aunque son poderosas, las expresiones regulares pueden volverse complejas y difíciles de manejar para patrones más complicados. Apunta a la legibilidad y considera métodos alternativos si tu regex se vuelve demasiado enredado.
