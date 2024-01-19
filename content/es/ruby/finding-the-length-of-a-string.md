---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la longitud de una cadena de texto (string) significa descubrir cuántos caracteres contiene. Los programadores lo hacen para resolver problemas que dependen de la longitud de la cadena, como validar datos de entrada o procesar textos.

## Cómo hacer:

Podemos determinar la longitud de una cadena utilizando el método `length` en Ruby:

```Ruby
cadena = "Hola Mundo"
puts cadena.length
```

La salida será `10`, dado que "Hola Mundo" tiene 10 caracteres contando los espacios.

Para cadenas con caracteres de doble byte como Japonés o Chino, usamos `bytesize`:

```Ruby
cadena = "こんにちは"
puts cadena.bytesize
```

La salida será `15`, dado que cada carácter en japonés toma 3 bytes.

## Descenso Profundo:

Los métodos para determinar la longitud de una cadena se incorporaron a Ruby desde su creación en 1995. A pesar de que `length` and `size` hacen la misma tarea, se añadió `size` por motivos de legibilidad al trabajar con colecciones. `bytesize` fue introducido para manejar caracteres Unicode que pueden ocupar más bytes que los caracteres ASCII.

Alternativamente, puedes usar el método `size` en lugar de `length`:

```Ruby
cadena = "Hola Mundo"
puts cadena.size
```

Este código también devolverá `10`, y puedes usar ambas funciones según tus necesidades y preferencias.

Sobre la implementación, Ruby almacena las cadenas como una colección de bytes más la codificación asociada. Por lo tanto, los métodos `length` y `size` simplemente cuentan el número de caracteres sin tener en cuenta el número de bytes que ocupan.

## Consultar También:

- Documentación oficial de Ruby sobre la clase String: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Umbral para la longitud de la cadena de Ruby: [https://stackoverflow.com/questions/4882374/whats-the-maximum-length-of-a-ruby-string](https://stackoverflow.com/questions/4882374/whats-the-maximum-length-of-a-ruby-string)
- Manipulaciones de cadenas en Ruby: [https://www.tutorialspoint.com/ruby/ruby_strings.htm](https://www.tutorialspoint.com/ruby/ruby_strings.htm)