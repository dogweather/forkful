---
title:                "Ruby: Convirtiendo una cadena a minúsculas"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Hay muchas razones por las cuales uno podría querer convertir una cadena (string) a minúsculas en Ruby. Puede ser para estandarizar el formato de una cadena que ingresó un usuario, para comparar dos cadenas de manera más precisa o simplemente para hacer que una cadena sea más fácil de leer.

## Cómo hacerlo:

Para convertir una cadena a minúsculas en Ruby, podemos usar el método `downcase`. Veamos un ejemplo de cómo usarlo en un programa:

```Ruby
cadena = "HOLA MUNDO"
puts cadena.downcase
```

Esto producirá la siguiente salida:

```Ruby
hola mundo
```

Tenga en cuenta que el método `downcase` devuelve una nueva cadena en minúsculas, por lo que si queremos modificar la cadena original, podemos usar el método `downcase!` con un signo de exclamación al final, como se muestra a continuación:

```Ruby
cadena = "HOLA MUNDO"
cadena.downcase!
puts cadena
```

La salida será:

```Ruby
hola mundo
```

## Profundizando en la conversión de cadenas a minúsculas:

Ahora que sabemos cómo convertir una cadena a minúsculas en Ruby, es importante entender cómo se realiza esta conversión internamente. En Ruby, cada carácter en una cadena tiene un código numérico asociado, conocido como código ASCII (American Standard Code for Information Interchange). Los caracteres en mayúsculas y minúsculas tienen números diferentes, por lo que cuando se llama al método `downcase`, Ruby simplemente iterará a través de cada carácter en la cadena y le restará 32 al número del código ASCII para convertirlo a minúscula.

Además, el método `downcase` no solo funciona con letras en inglés, sino que también puede manejar caracteres en otros idiomas, como el español. Por ejemplo:

```Ruby
cadena = "HOLA MUNDO"
cadena_español = "HASTA LUEGO"
puts cadena_español.downcase
```

Producirá la salida:

```Ruby
hasta luego
```

¡Ahora ya sabes cómo convertir cadenas a minúsculas en Ruby y cómo funciona este proceso internamente!

## Ver también

- [Ruby String Documentation](https://ruby-doc.org/core-2.5.0/String.html)
- [Ruby ASCII Codes](http://www.asciitable.com/)