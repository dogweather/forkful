---
title:                "Interpolación de cadenas de texto"
aliases:
- /es/arduino/interpolating-a-string/
date:                  2024-01-20T17:50:12.175783-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La interpolación de cadenas permite insertar valores de variables dentro de una cadena de texto, creando así una cadena compuesta dinámicamente. Programadores la usan porque simplifica la concatenación, mejora la legibilidad y facilita la localización de los textos.

## Cómo hacerlo:
Para interpolar una cadena en Arduino, usualmente concatenamos con el operador `+` o con la función `sprintf()`. Aquí van dos ejemplos breves:

```Arduino
char buffer[50];
int temp = 23;

// Usando el operador '+':
String message = "La temperatura es " + String(temp) + " grados Celsius.";
Serial.println(message);

// Usando sprintf():
sprintf(buffer, "La temperatura es %d grados Celsius.", temp);
Serial.println(buffer);
```

Salidas esperadas:
```
La temperatura es 23 grados Celsius.
La temperatura es 23 grados Celsius.
```

## Profundización
Históricamente, el lenguaje C, en el cual se basa Arduino, utiliza funciones como `sprintf()` para formatear cadenas. No obstante, en otros idiomas más modernos, la interpolación es más directa, como Ruby o Python que usan `#{variable}` o `f"texto {variable}"`, respectivamente. En Arduino, nos limitamos al estilo C por restricciones de memoria y porque el lenguaje está orientado al rendimiento en sistemas embebidos. Alternativas a `sprintf()` incluyen la concatenación manual o usar `String` y sus métodos, pero cada opción tiene implicaciones en el uso de la memoria y rendimiento, algo a tener en cuenta al programar microcontroladores.

## Ver También
- La referencia oficial de Arduino sobre Strings: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Un tutorial sobre `sprintf()` en C: http://www.cplusplus.com/reference/cstdio/sprintf/
- Documentación de Arduino sobre `Serial.print()`: https://www.arduino.cc/reference/en/language/functions/communication/serial/print/
