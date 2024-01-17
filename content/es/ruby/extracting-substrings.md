---
title:                "Extrayendo subcadenas"
html_title:           "Ruby: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas es una técnica común utilizada por los programadores para obtener parte de una cadena de texto más grande. Esto puede ser útil para realizar diferentes operaciones, como buscar una palabra clave específica o manipular datos específicos en una cadena de texto.

## Cómo:
Al trabajar con Ruby, hay varias opciones para extraer subcadenas. Una forma común es usar el método `slice`, que toma dos argumentos: el índice de inicio y el índice final (opcional). El siguiente ejemplo muestra cómo extraer una subcadena de la cadena "Hola mundo" utilizando el método `slice`:

```Ruby
texto = "Hola mundo"
puts texto.slice(0, 4)
```
Esto imprimirá "Hola" en la consola. El primer argumento (0) es el índice de inicio y el segundo argumento (4) es el índice final, que indica hasta dónde se extraerá la subcadena.

Si solo se proporciona un argumento, el método `slice` extraerá la subcadena desde ese índice hasta el final de la cadena. Por ejemplo:

```Ruby
texto = "Hola mundo"
puts texto.slice(5)
```
Esto imprimirá "mundo" en la consola.

## Profundizando:
El método `slice` es solo una forma de extraer subcadenas en Ruby. También puede usar el método `substring` o `substr`, que funcionan de manera similar al método `slice` pero pueden tomar argumentos diferentes. También hay otras formas de lograr el mismo resultado, como la indexación de cadenas de texto utilizando corchetes.

Para obtener más información sobre cómo extraer subcadenas en Ruby y sus diferentes métodos, puede consultar la documentación oficial de Ruby o las numerosas guías en línea disponibles.

## Ver también:
- [Documentación oficial de Ruby: Métodos de cadenas de texto](https://ruby-doc.org/core-3.0.0/String.html)
- [Guía de subtarea de cordoba en Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Ruby para programadores de Python: Strings](https://www.ruby-lang.org/es/documentation/ruby-from-other-languages/to-ruby-from-python/#strings)