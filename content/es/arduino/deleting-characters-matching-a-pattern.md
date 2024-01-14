---
title:    "Arduino: Eliminando caracteres que coinciden con un patrón"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón en Arduino?

Borrar caracteres que coinciden con un patrón puede ser útil en muchas situaciones. Por ejemplo, si estás trabajando con un sensor que envía datos en un formato específico y solo necesitas ciertos valores, puedes borrar los caracteres que no necesitas antes de almacenar o procesar los datos.

## Cómo hacerlo en Arduino

Borrar caracteres que coinciden con un patrón en Arduino es bastante sencillo. Primero, definimos una cadena de texto que contenga los datos que queremos limpiar. Luego, utilizamos la función `replace()` para reemplazar los caracteres que coinciden con un patrón con una cadena vacía. Por último, imprimimos la cadena resultante para verificar que se hayan eliminado los caracteres no deseados.

```arduino
String datos = "12/03/2021#Datos de Temperatura#27.8#35.2";
datos.replace("#Datos de Temperatura#", "");
Serial.println(datos);
```

La salida de este ejemplo sería: `12/03/2021#27.8#35.2`, donde hemos eliminado la cadena "#Datos de Temperatura#".

## Profundizando en el borrado de caracteres por coincidencia de patrón

La función `replace()` acepta dos parámetros: el carácter o patrón que queremos reemplazar y el carácter o cadena por el que lo queremos reemplazar. Podemos utilizar esta función varias veces para eliminar múltiples patrones en una cadena de texto.

Además, existe otra función llamada `replaceAll()` que simplemente reemplaza todas las coincidencias de un patrón sin importar su posición en la cadena.

Otra forma de borrar caracteres por coincidencia de patrón es utilizando expresiones regulares. Esto puede ser útil cuando el patrón es más complejo o cuando queremos hacer reemplazos más específicos.

## Ver también

- [Tutorial de Arduino](https://www.arduino.cc/en/Tutorial/HomePage)
- [Documentación oficial de la función `replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Documentación oficial de la función `replaceAll()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replaceall/)
- [Expresiones regulares en Arduino](https://playground.arduino.cc/Code/Regexp)