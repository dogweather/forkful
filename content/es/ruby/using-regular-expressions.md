---
title:    "Ruby: Utilizando expresiones regulares"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Ruby

Las expresiones regulares son una herramienta poderosa en la programación de Ruby que nos permiten buscar y manipular patrones de texto de forma eficiente. Pueden ser útiles en tareas como la validación de datos de entrada, o en la búsqueda y reemplazo de cadenas de texto en un archivo. Aprender a utilizar expresiones regulares en Ruby puede ahorrarle tiempo y esfuerzo en sus proyectos de programación.

## Cómo utilizar expresiones regulares en Ruby

Para utilizar expresiones regulares en Ruby, primero debe crear un objeto Regex utilizando el método `.new` y proporcionarle un patrón de búsqueda entre dos barras diagonales (`/patrón/`). Por ejemplo:

```Ruby
regex = /hola/
```

Luego, puede utilizar varios métodos de la clase String, como `.match()`, `.scan()` y `.sub()`, para buscar, extraer y reemplazar patrones utilizando su objeto regex. Veamos algunos ejemplos:

```Ruby
texto = "¡Hola mundo!"
texto.match(regex) # => #<MatchData "hola">
texto.scan(regex) # => ["hola"]
texto.sub(regex, "adiós") # => "¡adiós mundo!"
```

Como se puede ver, el método `.match()` nos devuelve un objeto MatchData que contiene la primera coincidencia encontrada en el texto, mientras que el método `.scan()` nos devuelve un array con todas las coincidencias. Por otro lado, el método `.sub()` nos permite reemplazar la primera coincidencia encontrada por una cadena especificada.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en Ruby tienen una amplia gama de funcionalidades y opciones que pueden ser combinadas de formas interesantes y útiles. Por ejemplo, puede utilizar los modificadores `i` (ignorar mayúsculas/minúsculas) y `m` (cambiar el significado de `^` y `$` para buscar patrones en múltiples líneas) para hacer búsquedas más flexibles y precisas. También es posible utilizar los metacaracteres `\d` (dígitos), `\w` (caracteres alfanuméricos) y `\s` (espacios en blanco) para crear patrones más específicos. Como en todo, la práctica y la experimentación harán que se sienta más cómodo con el uso de expresiones regulares en Ruby.

## Ver también

- [Tutorial de Expresiones Regulares en Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- [La documentación oficial de Ruby sobre expresiones regulares](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Ruby Monk - Expresiones Regulares](https://rubymonk.com/learning/books/1-ruby-primer/chapters/1-regular-expressions-in-ruby/lessons/4-regular-expressions-in-context)

¡Esperamos que este artículo le haya ayudado a entender mejor cómo utilizar expresiones regulares en Ruby! Siga practicando y experimentando para mejorar sus habilidades en el uso de esta poderosa herramienta. ¡Hasta la próxima!