---
title:                "Concatenación de cadenas de texto"
aliases:
- /es/ruby/concatenating-strings.md
date:                  2024-01-20T17:35:59.211754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La concatenación de cadenas es unir dos o más cadenas de texto en una sola. Los programadores la utilizan para combinar mensajes, construir datos dinámicamente o simplemente organizar información de forma legible.

## How to:
La concatenación en Ruby es pan comido. Aquí van unos ejemplos:

```ruby
# Usando el operador '+'
saludo = "Hola, " + "mundo!"
puts saludo  # => Hola, mundo!

# Concatenando variables
nombre = "Pedro"
mensaje = "¿Qué tal, " + nombre + "?"
puts mensaje  # => ¿Qué tal, Pedro?

# Usando la interpolación de cadenas con #{}
saludo = "Hola"
nombre = "Ana"
mensaje_completo = "#{saludo}, #{nombre}!"
puts mensaje_completo  # => Hola, Ana!

# Usando el método 'concat'
saludo = "¡Buenos "
saludo.concat("días!")
puts saludo  # => ¡Buenos días!

# Usando shovel operator '<<'
adj = "increíble"
frase = "Este día es "
frase << adj
puts frase  # => Este día es increíble
```

## Deep Dive
La concatenación de cadenas es tan antigua como los propios lenguajes de programación. En Ruby, esta acción es muy intuitiva y tenemos varias maneras de hacerla.

Históricamente, los programadores siempre han necesitado unir información textual. Por ello, la mayoría de los lenguajes de programación cuentan con esta funcionalidad.

Además de los métodos anteriores, existen alternativas como `Array#join`. Es útil cuando tenemos un arreglo de cadenas que deseamos unir:

```ruby
palabras = ['Ruby', 'es', 'divertido']
frase = palabras.join(' ')
puts frase  # => Ruby es divertido
```

Sobre la implementación, usar `+` o `concat` para concatenar muchas cadenas puede resultar ineficiente, ya que crea un nuevo objeto de cadena en cada operación. Por otro lado, `<<` y la interpolación son más eficientes y generalmente preferidos en tales casos.

## See Also
Para profundizar tus conocimientos, aquí tienes algunos enlaces:

- [Ruby Documentation on Strings](https://ruby-doc.org/core-3.1.0/String.html) (en inglés)

Estos recursos te ayudarán a entender mejor la concatenación de cadenas en Ruby y a conocer prácticas óptimas.
