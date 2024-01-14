---
title:    "Ruby: Capitalizando una cadena"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto es una tarea común en la programación. Al convertir todas las letras de una cadena a mayúsculas, podemos dar un formato uniforme a nuestros datos y facilitar su manipulación. A continuación, veremos cómo realizar esta operación en Ruby.

## Cómo hacerlo

La forma más simple de capitalizar una cadena de texto en Ruby es utilizando el método `upcase`, que se encuentra disponible en todos los objetos de tipo `String`. Este método devuelve una nueva cadena con todas las letras en mayúsculas.

```Ruby
"hola mundo".upcase
# => "HOLA MUNDO"
```

Si queremos capitalizar solo la primera letra de una cadena, podemos utilizar el método `capitalize`.

```Ruby
"hola mundo".capitalize
# => "Hola mundo"
```

También existe el método `upcase!`, que modifica la cadena original en lugar de devolver una nueva cadena.

```Ruby
texto = "hola mundo"
texto.upcase!
puts texto
# => "HOLA MUNDO"

puts texto.upcase!
# => "HOLA MUNDO"
```

También podemos utilizar el método `gsub` junto con una expresión regular para capitalizar cada palabra dentro de una cadena.

```Ruby
"hola mundo".gsub(/\b\w/){|l| l.upcase}
# => "Hola Mundo"
```

## Profundizando

Mientras que los métodos mencionados anteriormente funcionan perfectamente para cadenas en inglés o español, también existen métodos más avanzados para capitalizar cadenas en idiomas con reglas de capitalización más complejas. Por ejemplo, en alemán la letra "ß" se convierte en "SS" al capitalizar una palabra.

Para estos casos más específicos, podemos utilizar la gema `unicode`, que incluye el método `upcase_first_letter` que maneja correctamente estos casos especiales.

```Ruby
require 'unicode'
Unicode.upcase_first_letter("Straße")
# => "Straße"
```

## Ver también

- [Métodos de String en Ruby](https://ruby-doc.org/core-2.7.1/String.html)
- [Gema Unicode](https://github.com/knu/ruby-unicode)
- [Reglas de capitalización en diferentes idiomas](https://www.rosettacode.org/wiki/Title_case)