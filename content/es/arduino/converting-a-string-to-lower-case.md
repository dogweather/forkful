---
title:    "Arduino: Convirtiendo una cadena a minúsculas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por Qué?

Una tarea común en programación es convertir una cadena de texto a minúsculas. Esto puede ser necesario cuando se trabaja con entradas de usuario o se necesita comparar cadenas sin importar las mayúsculas o minúsculas. En este artículo, te explicaré cómo realizar esta conversión en Arduino.

## Cómo Hacerlo

La conversión de una cadena a minúsculas en Arduino es sencilla utilizando la función `toLowerCase()`. Primero, declaramos una cadena de texto y la asignamos a una variable:

```Arduino
String texto = "Hola Mundo";
```

Luego, utilizamos la función `toLowerCase()` y sobrescribimos la variable `texto` con el resultado:

```Arduino
texto = texto.toLowerCase();
```

¡Y eso es todo! Ahora la variable `texto` contendrá la cadena "hola mundo". Veamos un ejemplo completo:

```Arduino
String texto = "Hola Mundo";
texto = texto.toLowerCase();
Serial.println(texto);
```

La salida en el monitor serie sería:

```
hola mundo
```

## Profundizando

La función `toLowerCase()` se basa en las normas establecidas por la tabla ASCII. Esto significa que solo funcionará con caracteres alfabéticos en inglés. Si deseas convertir cadenas con caracteres especiales o de otros idiomas, deberás utilizar una librería que proporcione esta funcionalidad.

Además, es importante tener en cuenta que la función `toLowerCase()` no modifica la cadena original, sino que devuelve una nueva cadena en minúsculas. Por lo tanto, si deseas utilizar la cadena en minúsculas, deberás asignarla a una nueva variable o sobrescribirla en la original.

## Ver También

- [Documentación oficial de la función toLowerCase() en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Ejemplos de uso de la función toLowerCase()](https://www.arduino.cc/en/Tutorial/UsingAsciiTable)