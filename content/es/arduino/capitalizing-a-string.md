---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena significa convertir el primer caracter de la cadena en mayúscula, mientras que el resto permanece en minúscula. Los programadores lo hacen para mejorar la legibilidad y el formato de los textos en sus programas.

## Cómo hacerlo:

Aquí tienes un sencillo ejemplo de cómo capitalizar una cadena en Arduino:

```Arduino
String cadena = "hola mundo";
cadena.setCharAt(0, toupper(cadena.charAt(0)));
Serial.println(cadena);
```
La salida será: `Hola mundo`.

## Un Viaje Profundo:

El proceso de capitalización de cadenas tiene sus raíces en las primeras computadoras mainframe, donde la correcta escritura y formato era fundamental para que el sistema funcionara correctamente. 

En Arduino, la forma más común de capitalizar una cadena es usando la función setCharAt() en combinación con toupper(). Sin embargo, también puedes usar toUpperCase() si deseas convertir toda la cadena en mayúsculas.

Por último, recuerda que estas funciones consumirán cierta cantidad de memoria y tiempo de procesamiento, así que úsalas solo cuando sea necesario para mantener tu programa eficiente.

## Ver También:

Aquí tienes algunos enlaces útiles para obtener más información sobre las cadenas en Arduino. 

1. [Arduino - String setCharAt()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/setcharat/)
2. [Arduino - String charAt() ](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charat/)
3. [Arduino - String toUpperCase() ](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)