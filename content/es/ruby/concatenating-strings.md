---
title:    "Ruby: Concatenando cadenas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

En este artículo, hablaremos sobre cómo concatenar cadenas en Ruby. Así que si eres un programador de Ruby o simplemente estás interesado en aprender más sobre este lenguaje de programación, ¡sigue leyendo!

## Why (Por qué)

El proceso de concatenación de cadenas puede ser muy útil al trabajar con texto en Ruby. Al combinar varias cadenas de texto en una sola, podemos crear mensajes personalizados, formularios de correo electrónico o incluso generar texto en tiempo real para los usuarios de nuestra aplicación. En resumen, concatenar cadenas nos permite manipular y mostrar texto de una manera más eficiente y efectiva.

## How To (Cómo hacerlo)

La sintaxis básica para concatenar cadenas en Ruby es utilizando el operador `+` entre las cadenas que queremos unir. Por ejemplo:

```Ruby
nombre = "Juan"
apellido = "González"

nombre_completo = nombre + " " + apellido
puts nombre_completo
# Output: Juan González
```

En este ejemplo, creamos dos variables `nombre` y `apellido` y luego las unimos utilizando el operador `+`, seguido de una cadena vacía para agregar un espacio entre las palabras. Finalmente, imprimimos el resultado utilizando `puts` y obtenemos el nombre completo, "Juan González".

También podemos utilizar el método `concat` para concatenar cadenas. Este método nos permite unir varias cadenas en una sola línea de código. Por ejemplo:

```Ruby
saludo = "¡Hola"
nombre = "María"
signo = "!"

saludo.concat(" ", nombre, signo)
puts saludo
# Output: ¡Hola María!
```

Aquí, utilizamos el método `concat` junto con las variables `saludo`, `nombre` y `signo` para generar el saludo completo, "¡Hola María!".

## Deep Dive (Profundizando)

Además de utilizar el operador `+` y el método `concat`, también podemos concatenar cadenas utilizando el método `<<`, que se conoce como "shovel operator" en Ruby. Este método funciona de manera similar al operador `+=` en otros lenguajes de programación. Por ejemplo:

```Ruby
saludo = "¡Hola"
nombre = "Pedro"
signo = "!"

saludo << " " << nombre << signo
puts saludo
# Output: ¡Hola Pedro!
```

También vale la pena mencionar que el método `<<` modifica directamente la cadena a la que se está agregando, mientras que `concat` y `+` devuelven una nueva cadena concatenada.

Otra forma de concatenar cadenas es utilizando el método `interpolate` junto con el símbolo `#{ }`. Este método nos permite incluir variables en medio de una cadena. Por ejemplo:

```Ruby
nombre = "Ana"
edad = 25

mensaje = "¡Hola #{nombre}! Tienes #{edad} años."
puts mensaje
# Output: ¡Hola Ana! Tienes 25 años.
```

En este ejemplo, utilizamos las variables `nombre` y `edad` dentro de la cadena utilizando la sintaxis `#{ }` para obtener el mensaje personalizado, "¡Hola Ana! Tienes 25 años."

## See Also (Ver también)

Esperamos que este artículo te haya ayudado a comprender mejor cómo concatenar cadenas en Ruby. Si quieres aprender más sobre strings y métodos de manipulación de texto, te recomendamos los siguientes enlaces:

- [Documentación de Ruby sobre strings] (https://ruby-doc.org/core-2.7.0/String.html)
- [Tutorial de Ruby sobre strings] (https://www.rubyguides.com/2019/05/ruby-string-methods/)
- [Ruby String Methods Cheat Sheet] (https://www.freecodecamp.org/news/ruby-string-methods-cheat-sheet/)