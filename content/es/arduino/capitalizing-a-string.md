---
title:                "Arduino: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al trabajar con strings en Arduino, nos encontramos en la necesidad de capitalizar ciertas palabras para que se muestren de manera correcta en nuestras pantallas LCD o en otros dispositivos. Esto puede ser especialmente útil cuando queremos mostrar títulos o nombres de manera estética y profesional. En este artículo, aprenderemos cómo capitalizar un string en Arduino para mejorar la presentación de nuestros proyectos.

## Cómo hacerlo

Existen varias formas de capitalizar un string en Arduino. A continuación, mostraremos dos ejemplos:

```
Arduino String texto = "hola mundo";
texto.toUpperCase();
```

Este código, utilizando el método `toUpperCase()`, convertirá todo el string `texto` a mayúsculas y mostrará en pantalla "HOLA MUNDO".

```
Arduino String texto = "hola mundo";
texto.replace(0,1,"H");
```

En este segundo ejemplo, utilizamos el método `replace()` para reemplazar el primer carácter del string por la letra "H". Esto también producirá la salida "Hola mundo".

Estos son solo dos ejemplos, pero hay muchas otras formas de capitalizar un string en Arduino. ¡Pruébalo tú mismo y encuentra la que mejor se adapte a tus necesidades!

## Profundizando

Si quieres saber más sobre cómo funcionan estos métodos, es importante entender que en Arduino, los strings son en realidad objetos de la clase `String`. Esto significa que podemos utilizar otros métodos de la clase para manipular nuestros strings. Por ejemplo, también podríamos utilizar el método `charAt()` y `setChar()` para cambiar caracteres específicos dentro del string. Además, existen librerías específicas que pueden ayudarnos a capitalizar strings de una manera más avanzada, como por ejemplo [StringCase.h](https://github.com/casey/ArduinoStringObject) o [TextManipulator.h](https://github.com/boyska/TextManipulator).

En resumen, hay múltiples opciones para capitalizar un string en Arduino y la elección dependerá de tus necesidades y conocimientos sobre programación. Sin embargo, con estas herramientas y librerías, es posible lograr un resultado profesional y atractivo en tus proyectos.

## Ver también

- [String Manipulation in Arduino](https://www.arduino.cc/en/Tutorial/StringObject)
- [String Functions in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino String Class](https://www.arduino.cc/en/Reference/StringObject)
- [TextManipulator.h library on Github](https://github.com/boyska/TextManipulator)
- [StringCase.h library on Github](https://github.com/casey/ArduinoStringObject)