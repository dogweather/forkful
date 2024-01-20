---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Extraer subcadenas en Ruby implica seleccionar y obtener una parte o fragmento de una cadena de texto. Los programadores lo hacen para manipular o analizar solamente una porción del texto, lo cual es esencial en tareas como procesamiento de texto, análisis de datos y manipulación de cadenas.

## ¿Cómo se hace?

Ruby ofrece diferentes métodos para extraer subcadenas. Podemos usar el método `slice`, `[]` o `substring`. Aquí van algunos ejemplos:

```Ruby
cadena = "Hola, Mundo!"

# Extrayendo substrings con `slice`
puts cadena.slice(0,4)     #=> "Hola"
puts cadena.slice(6,5)     #=> "Mundo"
puts cadena.slice(2..4)    #=> "la,"

# Extrayendo substrings con `[]`
puts cadena[0,4]           #=> "Hola"
puts cadena[6,5]           #=> "Mundo"
puts cadena[2..4]          #=> "la,"

# Extrayendo substrings con `substring` (es alias de `slice`)
puts cadena.substring(0,4) #=> "Hola"
puts cadena.substring(6,5) #=> "Mundo"
puts cadena.substring(2..4)#=> "la,"
```

## Un vistazo más profundo

Históricamente, los métodos `slice` y `[]` han formado parte del núcleo de Ruby desde sus primeras versiones. `substring` se agregó después, como un alias para `slice`, brindando consistencia con otros lenguajes de programación.

En cuanto a las alternativas, existe el método `split`, que puede ser útil si las subcadenas que deseas extraer están separadas por un caracter específico.

```Ruby
cadena = "Hola, Mundo!"
puts cadena.split(', ') #=> ["Hola", "Mundo!"]
```

En cuanto a detalles de implementación, tanto `slice` como `[]` y `substring` son extremadamente eficientes, dado que Ruby implementa las cadenas de texto como arreglos de caracteres.

## Ver También

- [Documentación oficial de Ruby sobre las cadenas de texto](https://ruby-doc.org/core-2.7.0/String.html)
- [Tutorial de Ruby sobre el manejo de cadenas](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Extraer subcadenas en Ruby: tutorial y ejemplos](https://www.rubyguides.com/2018/01/ruby-string-methods/)