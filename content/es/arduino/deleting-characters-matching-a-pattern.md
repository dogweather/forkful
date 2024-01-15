---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Arduino: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A veces, en nuestro código de Arduino, es posible que tengamos una cadena de texto o un conjunto de datos con caracteres no deseados. Ya sea por errores de entrada de datos o por la necesidad de limpiar nuestra información, puede ser útil saber cómo eliminar los caracteres que coinciden con un patrón en particular.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Arduino, podemos utilizar la función `replace()` de la librería `String`. Esta función toma dos parámetros: el primer parámetro es el patrón que queremos reemplazar y el segundo es el carácter que se utilizará como reemplazo. Por ejemplo, si queremos eliminar todas las letras "a" de una cadena de texto, podemos usar `replace("a", "")` para reemplazar todas las "a" con una cadena vacía, eliminándolas del texto.

```
Arduino void setup(){
    String texto = "Hola a todos";
    texto.replace("a", "");
    Serial.println(texto);
}
```

La salida de este código sería "Hol todos", ya que todas las "a" fueron eliminadas del texto original.

Otra función útil para eliminar caracteres es `remove()`, que nos permite eliminar un rango de caracteres en una cadena de texto. Para utilizar esta función, debemos especificar el índice del primer carácter a eliminar y el número de caracteres a eliminar. Por ejemplo, si queremos eliminar los tres primeros caracteres de un texto, podemos usar `remove(0, 3)`.

```
Arduino void setup(){
    String texto = "12345";
    texto.remove(0, 3);
    Serial.println(texto);
}
```

La salida sería "45", ya que se eliminaron los tres primeros caracteres de la cadena.

Si necesitamos eliminar caracteres que no necesariamente coinciden con un patrón, podemos utilizar un bucle `for` junto con la función `charAt()` para recorrer cada carácter de la cadena y eliminarlo según nuestras necesidades.

## Profundizando

Además de las funciones mencionadas, existen otras formas de eliminar caracteres en Arduino, como utilizar expresiones regulares o crear nuestras propias funciones que realicen la eliminación de caracteres. También es importante tener en cuenta que al trabajar con cadenas de texto, debemos tener en cuenta que cada carácter ocupa un byte de memoria, por lo que si estamos lidiando con cadenas muy largas, es posible que tengamos problemas de falta de memoria.

## Ver también

- Documentación oficial de la función `replace()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/
- Documentación oficial de la función `remove()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove/
- Tutorial de Arduino sobre manejo de cadenas de texto: https://www.arduino.cc/en/Tutorial/StringConstructors