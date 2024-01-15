---
title:                "Capitalizando una cadena"
html_title:           "Arduino: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por Qué

Escribir en mayúsculas es una tarea bastante común en la programación. Puede ser necesario para mostrar un mensaje en la pantalla, para convertir una entrada del usuario a mayúsculas o simplemente para mantener una consistencia en el formato de texto. En este artículo, aprenderás cómo capitalizar una cadena en el lenguaje de programación Arduino.

## Cómo Hacerlo

El proceso de capitalizar una cadena en Arduino es relativamente sencillo. Simplemente debes seguir los siguientes pasos:

1. Primero, declara una variable de tipo string y asígnale el valor de la cadena que quieras capitalizar. Por ejemplo: 

```
Arduino String mensaje = "hola mundo";
```

2. A continuación, utiliza el método "toUpperCase()" para convertir la cadena a mayúsculas. Esto se hace mediante el operador de punto "." después del nombre de la variable y luego escribiendo el método. Ejemplo:

```
mensaje.toUpperCase();
```

3. Finalmente, imprime el valor de la variable para verificar que la cadena haya sido convertida a mayúsculas:

```
Serial.println(mensaje);
```

El resultado en la pantalla debería ser "HOLA MUNDO".

## Profundizando

El método "toUpperCase()" utilizado en el ejemplo anterior es una función incorporada de la clase String en Arduino. Esto significa que puede ser utilizado en cualquier variable de tipo string sin necesidad de declararlo previamente.

Además de "toUpperCase()", también existe el método "toLowerCase()" que convierte una cadena a minúsculas. Estos métodos son especialmente útiles en casos en los que se necesite validar la entrada del usuario o comparar cadenas sin importar si están en mayúsculas o minúsculas.

También es importante tener en cuenta que el método "toUpperCase()" no altera el valor de la variable original, sino que crea una nueva cadena con los caracteres convertidos a mayúsculas. Por lo tanto, si deseas guardar el valor de la cadena en mayúsculas, debes asignar el resultado del método a una nueva variable.

## Ver También

Más información sobre el uso de variables de tipo string en Arduino: [https://www.arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

Métodos incorporados de la clase String: [https://www.arduino.cc/reference/en/language/variables/data-types/string/methods/](https://www.arduino.cc/reference/en/language/variables/data-types/string/methods/)