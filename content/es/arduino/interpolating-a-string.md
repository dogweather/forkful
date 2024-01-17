---
title:                "Interpolando una cadena"
html_title:           "Arduino: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Interpolar una cadena de caracteres es una técnica común en programación donde se insertan variables o valores numéricos en una cadena de caracteres. Esto permite que el texto sea más dinámico y adaptativo a diferentes situaciones. Los programadores lo hacen para facilitar la escritura de código y para crear cadenas de caracteres más expresivas y fáciles de leer.

## Cómo:
```
Arduino.println("¡Hola, mi nombre es ${nombre} y tengo ${edad} años!");
```

En este ejemplo, la cadena de caracteres "¡Hola, mi nombre es" y "y tengo años!" permanecen igual, mientras que las variables `${nombre}` y `${edad}` se reemplazan con los valores que tengan asignados en el código. Así, si la variable `nombre` tiene el valor de "Juan" y la variable `edad` tiene el valor de "20", el resultado será "¡Hola, mi nombre es Juan y tengo 20 años!".

## Inmersión Profunda:
Interpolar cadenas de caracteres se ha vuelto una técnica muy popular en los últimos años debido a su eficacia y facilidad de uso. Algunos otros métodos alternativos incluyen el uso de concatenación de cadenas de caracteres y formateo de cadenas de caracteres. Sin embargo, interpolar una cadena de caracteres es más eficiente y ofrece una sintaxis más elegante.

En la implementación de Arduino, el símbolo `$` se utiliza para indicar que se va a interpolar una variable o valor en una cadena de caracteres. Además, dentro de la cadena de caracteres, se deben utilizar las llaves `{}` para delimitar la variable o valor que se desea interpolar.

## Ver También:
- Tutorial de interpolación de cadenas de caracteres en Arduino: https://www.instructables.com/id/Arduino-Tutorial-Interpolation-in-Strings/
- Documentación oficial de Arduino sobre interpolar cadenas de caracteres: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/interp/
- Artículo sobre cómo mejorar la legibilidad del código con la interpolación de cadenas de caracteres: https://medium.com/@csell5/interpolating-strings-to-improve-readability-ebf3c21b68e6