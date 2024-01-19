---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenación de Cadenas en Ruby

## Qué y Por qué?
La concatenación de cadenas es el proceso de unir dos o más cadenas de texto en una sola. Los programadores lo hacen para manipular y presentar datos de una manera más legible o formativa.

## Cómo hacerlo:
Aquí te muestro cómo unir cadenas en Ruby.

```Ruby 
# Uniendo usando '+'
nombre = "Juan"
apellido = "Perez"
nombre_completo = nombre + ' ' + apellido
puts nombre_completo # Salida: "Juan Perez"

# Concatenando usando '<<'
nombre = "Juan"
nombre << " Perez"
puts nombre # Salida: "Juan Perez"

# Interpolando cadenas
nombre = "Juan"
apellido = "Perez"
nombre_completo = "#{nombre} #{apellido}"
puts nombre_completo # Salida: "Juan Perez"
``` 

## Profundización
Historicamente, '+' fue el primer método para concatenar cadenas en Ruby, pero puede ser lento para cadenas largas o un gran número de concatenaciones debido a su necesidad de crear nuevas instancias de String. Después se introdujo '<<', que modifica la cadena original, siendo más rápido y eficiente en memoria. La interpolación de cadenas ('#{}') se introdujo después proporcionando una forma más clara y flexible de concatenar y formatear cadenas.

Alternativamente, puedes usar 'concat()' o 'join()', pero el uso de estos depende de las necesidades específicas de tu código.

```Ruby 
# Usando 'concat()'
nombre = "Juan"
nombre.concat(" Perez")
puts nombre # Salida: "Juan Perez"

# Usando 'join()'
nombre = ["Juan", "Perez"]
nombre_completo = nombre.join(" ")
puts nombre_completo # Salida: "Juan Perez"
```

## Ver también
Para más información sobre el manejo de cadenas en Ruby, consulta los siguientes enlaces:
- Documentación oficial de Ruby sobre Strings: https://ruby-doc.org/core/String.html
- Comparación de rendimiento de los métodos de concatenación de cadenas: https://www.honeybadger.io/blog/ruby-string-concatenation/