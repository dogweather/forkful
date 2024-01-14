---
title:                "Ruby: Encontrando la longitud de una cadena"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Algunas veces cuando estamos programando en Ruby, necesitamos saber la longitud de una cadena de texto. Esto nos permite realizar diferentes manipulaciones y operaciones con los datos. En este artículo, te mostraré cómo encontrar la longitud de una cadena en Ruby y algunas cosas interesantes que puedes hacer con ella.

## Cómo Hacerlo
En Ruby, hay una forma muy fácil y directa de encontrar la longitud de una cadena. Simplemente usamos el método `length` seguido de la cadena a la que deseamos aplicarlo. Por ejemplo:

```Ruby
cadena = "Esto es una cadena de texto"
puts cadena.length # Output: 28
```

Como puedes ver, el método `length` nos devuelve el número de caracteres de la cadena. Incluso puede funcionar con cadenas más largas:

```Ruby
cadena_larga = "¡Wow, esta cadena tiene más de 50 caracteres!"
puts cadena_larga.length # Output: 51
```

¡Pero espera! ¿Qué pasa si tenemos una cadena vacía? Intentemos con esto:

```Ruby
cadena_vacia = ""
puts cadena_vacia.length # Output: 0
```

Como era de esperar, incluso si la cadena está vacía, el método `length` funciona y nos devuelve 0.

Ahora, ¿qué pasa con los caracteres especiales? En Ruby, se les conoce como "caracteres escapados" y se representan con una barra invertida. Por ejemplo, el carácter de nueva línea se escribe como `\n` y el tabulador como `\t`. Pero, ¿cómo afectan estos caracteres a la longitud de una cadena? Veamos un ejemplo:

```Ruby
cadena_espacios = "Unos espacios al final \t"
puts cadena_espacios.length # Output: 31
```

Aunque visualmente solo tenemos unos pocos caracteres al final, en realidad el carácter escapado `\t` cuenta como 4 caracteres, por lo tanto la longitud total de la cadena aumenta en 4.

## Buceo Profundo
Ahora que sabemos cómo encontrar la longitud de una cadena en Ruby, hablemos de algunas otras cosas interesantes que podemos hacer con ella.

Una de esas cosas es el método `size`. Este método es equivalente a `length`, así que devuelve la misma cantidad de caracteres. Entonces, ¿por qué existe el método `size` si ya tenemos `length`? Bueno, la respuesta es que en Ruby hay algunas clases como Array y Hash que tienen tanto `length` como `size` y cada uno puede tener un comportamiento diferente. Por lo tanto, `size` se usa para mantener una coherencia sintáctica en el código.

Además, también hay otros métodos como `bytesize` y `each_char`. El primero nos devuelve el tamaño en bytes de la cadena y el segundo nos permite iterar sobre cada uno de los caracteres de la cadena. ¡Puedes investigar más sobre estos métodos y descubrir todas las posibilidades que ofrecen!

## Ver también
- [Documentación oficial de Ruby sobre el método `length`](https://ruby-doc.org/core-2.7.1/String.html#method-i-length)
- [Stack Overflow: Diferencias entre `length` y `size` en Ruby](https://stackoverflow.com/questions/2590355/what-is-the-difference-between-size-and-length-in-ruby/2590378)
- [RubyGuides: Tutorial completo sobre cadenas en Ruby](https://www.rubyguides.com/2019/04/ruby-strings/)