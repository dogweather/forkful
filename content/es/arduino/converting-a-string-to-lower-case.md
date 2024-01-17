---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Arduino: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué & por qué?

"Convertir una cadena de texto a minúsculas" es un proceso en el que se transforma una cadena de caracteres a una versión en la que todas las letras se encuentran en minúsculas. Este proceso es comúnmente utilizado por programadores para estandarizar los datos y facilitar las comparaciones entre diferentes cadenas de texto. 

## Cómo hacerlo:

```Arduino
String texto = "Hola Mundo"; 
//Creamos una cadena de texto con mayúsculas y minúsculas

texto.toLowerCase(); 
//Convertimos la cadena de texto a minúsculas

Serial.println(texto); 
//Imprimimos la cadena de texto convertida a la consola serial

```

El resultado de este código sería:
```
hola mundo
```

## Profundizando:

### Contexto histórico:

La transformación de cadenas de texto a minúsculas es un proceso que se ha utilizado desde los inicios de la informática. Antes de la existencia de los lenguajes de programación modernos, los primeros programadores tenían que realizar esta tarea de forma manual utilizando herramientas como la máquina de escribir o el lápiz y papel.

### Alternativas:

Además de la función `.toLowerCase()` utilizada en el ejemplo, también existen otras formas de convertir una cadena de texto a minúsculas en Arduino. Una de ellas es utilizando la biblioteca `String` y la función `.toLowerCase()`. También se puede hacer utilizando la función `String.copy()` junto con la función `tolower()` de la biblioteca `ctype.h`.

### Detalles de implementación:

La conversión de una cadena de texto a minúsculas se basa en el uso de tablas de caracteres que contienen las equivalencias entre mayúsculas y minúsculas. Estas tablas se encuentran en bibliotecas como `ctype.h` y son utilizadas internamente por la función `.toLowerCase()` para realizar la conversión.

## Ver también:

- [Documentación oficial de Arduino sobre la función .toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Ejemplo de conversión de cadena de texto a minúsculas utilizando la biblioteca String](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Documentación sobre la biblioteca ctype.h](https://www.cplusplus.com/reference/cctype/)