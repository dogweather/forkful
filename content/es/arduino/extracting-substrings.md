---
title:                "Extrayendo subcadenas"
html_title:           "Arduino: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad importante en la programación que te permite manipular y utilizar partes específicas de una cadena de texto. Esto puede ser útil si quieres buscar ciertos patrones en una frase, o si necesitas dividir una cadena en secciones más pequeñas para realizar diferentes operaciones.

## Cómo hacerlo

Arduino tiene una función incorporada llamada `substring()` que te permite extraer subcadenas de una cadena de texto. Para utilizarla, sigue estos sencillos pasos:

1. Primero, declara una variable de tipo `String` que contenga la cadena de texto de la cual quieres extraer la subcadena.

   ```
   String texto = "Hola mundo";  // La cadena de texto de la que queremos extraer la subcadena
   ```

2. Luego, usa la función `substring()` especificando el índice de inicio y el número de caracteres que quieres extraer. En Arduino, los índices comienzan desde 0.

   ```
   String subcadena = texto.substring(5, 10);  // Extrae 5 caracteres a partir del índice 10
   ```

3. Ahora puedes hacer lo que quieras con tu subcadena, como imprimirlo en la pantalla LCD o compararlo con otra cadena.

   ```
   Serial.println(subcadena);  // Imprime "mundo" en el monitor serial
   ```

Puedes jugar con diferentes valores de índices y longitud de caracteres para obtener diferentes subcadenas.

## Una mirada más profunda

La función `substring()` en Arduino funciona de manera similar a la función `substring()` en otros lenguajes de programación, como Java. Toma dos parámetros: el índice de inicio y la longitud de la subcadena. Y devuelve una nueva cadena de caracteres.

Además, ten en cuenta que la función `substring()` no modifica la cadena original, sino que solo devuelve una copia de la subcadena extraída. Por lo tanto, si quieres modificar la cadena original, deberás asignar el resultado de `substring()` a la variable original.

## Ver también

- [Documentación oficial de Arduino sobre la función substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutorial sobre cómo usar la función substring() en Arduino](https://www.arduino.cc/en/Tutorial/TextStringSubstring)