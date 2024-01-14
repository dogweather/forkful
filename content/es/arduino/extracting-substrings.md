---
title:                "Arduino: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en aprender a programar con Arduino, seguramente ya has notado la enorme cantidad de posibilidades que ofrece esta plataforma. Y una de las funcionalidades más útiles para manipular cadenas de texto es la extracción de subcadenas. En este artículo, te explicaremos por qué es importante aprender a hacerlo y cómo puedes hacerlo tú mismo.

## Cómo hacerlo

Para extraer una subcadena de una cadena de texto en Arduino, utilizaremos la función `substring()`. Primero, definiremos la cadena de texto completa y luego usaremos esta función para especificar la posición de inicio y la longitud de la subcadena que deseamos extraer.

```Arduino
// Definir la cadena de texto completa
String texto = "¡Hola mundo!";

// Extraer la subcadena "mundo" a partir del cuarto carácter (índice 3) y de longitud 5
String subcadena = texto.substring(3, 5);

// Imprimir la subcadena en el monitor serie
Serial.println(subcadena);

// Resultado: mundo
```

Puedes personalizar la posición de inicio y la longitud de la subcadena según tus necesidades, lo que te permite manipular y utilizar cadenas de texto de manera más eficiente en tu programa de Arduino.

## Profundizando

Ahora que sabes cómo extraer subcadenas en Arduino, es importante que entiendas cómo funciona esta función y cómo puedes utilizarla en situaciones más complejas. La función `substring()` toma dos parámetros: la posición de inicio y la longitud de la subcadena. Sin embargo, si solo especificamos la posición de inicio, la subcadena resultante incluirá todos los caracteres restantes de la cadena original.

También puedes utilizar variables en lugar de números específicos para indicar la posición y la longitud. Esto te permite crear programas dinámicos que pueden manipular diferentes cadenas de texto sin tener que cambiar manualmente los valores.

## Ver también

- [Tutorial de Arduino para principiantes](https://www.arduino.cc/en/Guide/HomePage)
- [Documentación oficial de la función `substring()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Ejemplos de proyectos de Arduino](https://create.arduino.cc/projecthub/projects/tags/arduino)