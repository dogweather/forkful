---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La concatenación de cadenas es el proceso de unir dos o más cadenas. Los programadores lo hacen para combinar texto, por ejemplo, personalizar mensajes o generar instrucciones de formato.

## Cómo hacerlo:

A continuación se encuentra un código de ejemplo junto con el resultado esperado en bloques de código de Arduino:

```Arduino
String cadena1 = "Hola ";
String cadena2 = "Mundo!";
String cadena3 = cadena1 + cadena2;

Serial.begin(9600);
Serial.println(cadena3);
```

Esto imprimirá: 

```Arduino
Hola Mundo!
```

## Buceo profundo:

La concatenación de cadenas no es exclusiva de Arduino, está en casi todos los lenguajes de programación. Históricamente, existen múltiples maneras de hacerlo. En el contexto de Arduino, algunas alternativas podrían ser el uso de `strcat()` o `sprintf()`. No obstante, el operador + es el más sencillo y directo.

Un detalle de implementación importante a tener en cuenta es que demasiadas concatenaciones de cadenas pueden consumir rápidamente la memoria limitada de Arduino. Por lo tanto, se debe utilizar con precaución.

## Ver también:

Para más información, consulte los siguientes recursos:

- Documentación oficial de Arduino sobre String: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Stack Overflow para consejos prácticos de la comunidad: [stackoverflow.com/questions/tagged/arduino+string](https://stackoverflow.com/questions/tagged/arduino+string)