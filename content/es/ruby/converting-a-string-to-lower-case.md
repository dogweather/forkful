---
title:                "Ruby: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado manipular una cadena de texto en tu programa de Ruby? Puede que hayas notado que algunas cadenas están en mayúsculas y otras en minúsculas, y esto puede afectar el funcionamiento de tu código. Para evitar problemas y tener más control sobre tus cadenas de texto, es importante saber cómo convertir una cadena a minúsculas.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Ruby, puedes utilizar el método `.downcase`. Este método toma una cadena y devuelve una copia de ella en minúsculas. Veamos un ejemplo:

```Ruby
cadena = "ESTO ES UNA CADENA EN MAYÚSCULAS"
puts cadena.downcase
```

El resultado sería: `esto es una cadena en mayúsculas`. Como puedes ver, todas las letras se han convertido a minúsculas.

También puedes utilizar este método para convertir sólo una parte de una cadena a minúsculas. Por ejemplo:

```Ruby
nombre = "Juan"
apellido = "PÉREZ"

puts nombre.downcase + " " + apellido.downcase
```

El resultado sería: `juan pérez`. En este caso, sólo hemos convertido a minúsculas el nombre y apellido, pero no el resto de la cadena.

## Profundizando

El método `.downcase` es muy útil, pero es importante tener en cuenta que sólo convierte letras en mayúsculas a minúsculas. Esto significa que si tenemos caracteres especiales o números en nuestra cadena, no serán afectados por este método.

Por ejemplo:

```Ruby
cadena = "¡ESTO TIENE CARACTERES ESPECIALES: #@$%!"
puts cadena.downcase
```

El resultado sería: `¡esto tiene caracteres especiales: #@$%!`. El símbolo de exclamación y los caracteres especiales no fueron afectados por el método.

También es importante mencionar que este método es sensible a la codificación de tu programa. Si tienes una cadena en un idioma diferente al inglés, puede que los caracteres no sean convertidos correctamente. Para solucionar esto, puedes especificar la codificación al llamar al método.

## Ver también

- Documentación oficial de `.downcase`: https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase
- Métodos de manipulación de cadenas en Ruby: https://www.rubyguides.com/2015/11/ruby-string-methods/