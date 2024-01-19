---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La conversión de una fecha a una cadena (también conocida como "string") en programación implica transformar la representación de los datos de fecha a un formato de texto. Esto es útil cuando quieres manipular fechas como texto o deseas mostrar fechas de una forma más legible para los usuarios.

## Cómo hacerlo:

A continuación vamos a ver cómo convertir una fecha a cadena en Ruby:

```ruby
# Usaremos el módulo 'date'
require 'date'

# Creamos un objeto de fecha
fecha = Date.new(2021, 12, 31)

# Lo convertimos a cadena utilizando el método 'to_s'
cadena = fecha.to_s

# Imprimimos la cadena
puts cadena

# Resultado: "2021-12-31"
```

Como puedes ver, el objeto fecha se convierte a una cadena con formato año-mes-día.

## Análisis profundo:

(1) Desde sus inicios, Ruby ha facilitado trabajar con fechas y cadenas haciendo que los métodos de conversión sean simples y directos. El método `to_s` utilizado aquí es un ejemplo de esto.

(2) Si lo que buscas es una representación de fecha con un formato específico, puedes usar el método `strftime`. Observa el siguiente ejemplo:

```ruby
require 'date'

fecha = Date.new(2021, 12, 31)

cadena = fecha.strftime("%d/%m/%Y")

puts cadena

# Resultado: "31/12/2021"
```

(3) Implementación: Cuando llamas a `to_s` en un objeto Date, Ruby llama internamente al método `strftime` con el formato por defecto "%Y-%m-%d".

## Ver también:

Para más detalles y posibilidades de formato consulta la documentación oficial de Ruby:

- Módulo Date: https://ruby-doc.org/stdlib-3.0.3/libdoc/date/rdoc/Date.html
- Método 'strftime': https://ruby-doc.org/stdlib-3.0.3/libdoc/date/rdoc/Date.html#method-i-strftime