---
title:    "Arduino: Capitalizando una cadena."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

A veces en nuestros proyectos de Arduino, nos encontramos en la necesidad de capitalizar una cadena de texto. Ya sea para mejorar la legibilidad, para manipular mejor la información o simplemente porque así lo requiere nuestro programa. En este blog post, te explicaremos cómo capitalizar una string en Arduino.

## Cómo hacerlo

La forma más sencilla de capitalizar una string en Arduino es utilizando la función `capitalize()`. Esta función es parte de la librería `String` de Arduino y toma como parámetro la cadena de texto a capitalizar.

```Arduino
#include <String.h>

String texto = "hola, mundo!";
texto.capitalize();
Serial.println(texto);
```

El resultado de este código sería `Hola, mundo!`, ya que la función `capitalize()` cambia la primera letra de la string a mayúscula.

Otra opción es utilizar la función `toUpperCase()` en conjunto con un bucle `for` para iterar a través de cada letra de la string y convertir la minúscula a mayúscula.

```Arduino
String texto = "adiós";
for (int i = 0; i < texto.length(); i++) {
  texto.setCharAt(i, toUpperCase(texto.charAt(i)));
}
Serial.println(texto);
```

Esto nos daría como resultado `ADIOS`.

## Profundizando

Aunque ambas opciones son válidas para capitalizar una string en Arduino, es importante entender cómo funcionan para poder elegir la que mejor se adapte a nuestras necesidades.

La función `capitalize()` utiliza la tabla de caracteres ASCII para convertir la primera letra a mayúscula. Es importante tener en cuenta que esto solo funciona con letras que tienen sentido en mayúscula, por ejemplo, la letra "ñ" no se convertirá a "Ñ".

La función `toUpperCase()` también utiliza la tabla de caracteres ASCII, pero lo hace de forma más manual, cambiando cada letra a mayúscula por separado. Esto nos da más control a la hora de capitalizar, ya que podemos añadir excepciones o manipular la string de otras maneras antes de convertirla a mayúscula.

En general, ambas funciones son útiles para capitalizar una string en Arduino, pero es importante entender cómo funcionan para elegir la mejor opción en cada caso.

## Ver también

- [Función `capitalize()` en la documentación oficial de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/)
- [Información sobre la tabla de caracteres ASCII](https://en.wikipedia.org/wiki/ASCII)