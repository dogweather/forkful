---
title:    "Ruby: Encontrando la longitud de una cadena"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo nos encontramos con la necesidad de calcular la longitud de una cadena de texto. Esto puede ser útil en diferentes situaciones, como por ejemplo, al validar una entrada de usuario o al manipular datos en una base de datos. Por esta razón, es importante entender cómo encontrar la longitud de una cadena en Ruby.

## Cómo hacerlo

En Ruby, podemos encontrar la longitud de una cadena utilizando el método `length` o el método `size`. Ambos métodos devolverán el número de caracteres de la cadena. Veamos un ejemplo de cómo utilizar estos métodos en nuestro código:

```Ruby
cadena = "Hola, mundo!"

puts cadena.length
# Output: 12

puts cadena.size
# Output: 12
```

Como podemos ver en el código anterior, simplemente llamamos al método `length` o `size` en la cadena que queremos evaluar y luego imprimimos el resultado. Ambos métodos devuelven el mismo resultado, por lo que podemos utilizar cualquiera de ellos.

También es importante tener en cuenta que estos métodos sólo funcionan en cadenas de texto, por lo que si intentamos aplicarlos a otros tipos de datos, obtendremos un error.

## Profundizando

Ahora que sabemos cómo utilizar los métodos `length` y `size`, veamos cómo funcionan internamente. En Ruby, una cadena de texto es en realidad un objeto de la clase `String`. Esto significa que estos métodos son métodos de instancia disponibles en cualquier objeto de tipo `String`.

El método `length` simplemente devuelve el valor de la propiedad `length` del objeto `String`, que es el número de caracteres de la cadena. El método `size`, por otro lado, es una implementación de `length` que puede ser sobrescrita en clases que heredan de `String`.

También podemos utilizar la notación de corchetes para obtener la longitud de una cadena, como en el siguiente ejemplo:

```Ruby
cadena = "Hola, mundo!"

puts cadena[0..4].length
# Output: 5
```

Aquí, estamos utilizando la notación de corchetes para seleccionar una porción específica de la cadena y luego obtenemos su longitud utilizando el método `length`.

## Ver también

Para más información sobre los métodos `length` y `size` en Ruby, puedes consultar la [documentación oficial de Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-length) o [este artículo](https://www.rubyguides.com/2019/09/string-length-in-ruby/) de RubyGuides. También puedes aprender más sobre cadenas de texto en nuestro blog [Ruby en Acción](https://rubyenaccion.com/).

¡Esperamos que este artículo te haya sido útil para entender cómo encontrar la longitud de una cadena en Ruby! Si tienes alguna pregunta o comentario, no dudes en dejarnos un comentario abajo. ¡Hasta la próxima!