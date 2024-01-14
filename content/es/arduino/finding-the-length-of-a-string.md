---
title:    "Arduino: Encontrando la longitud de una cadena"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cuántos caracteres tiene una cadena de texto? En proyectos de Arduino, esto es una pregunta común ya que a menudo necesitamos manipular cadenas para mostrar datos en una pantalla LCD o para comunicarnos con otros dispositivos. Saber cómo encontrar la longitud de una cadena puede ser muy útil en estos casos.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Arduino, utilizaremos la función `strlen()` de la biblioteca de cadena de Arduino. Esta función toma una cadena como parámetro y devuelve la cantidad de caracteres en esa cadena. Vamos a ver un ejemplo:

```Arduino
char cadena[] = "¡Hola!";

int longitud = strlen(cadena);

Serial.println(longitud);
```

En este ejemplo, tenemos una cadena con la palabra "¡Hola!" y utilizamos la función `strlen()` para encontrar su longitud. La variable `longitud` almacenará el valor de 6, ya que esa es la cantidad de caracteres en la cadena.

## Profundizando

Para aquellos que quieren un conocimiento más profundo sobre cómo funciona `strlen()` en Arduino, aquí hay algunas cosas a tener en cuenta:

- `strlen()` cuenta solo los caracteres visibles en una cadena, es decir, letras, números, símbolos, etc. No cuenta espacios en blanco, tabulaciones o caracteres de escape.
- Si utilizas caracteres no ASCII en una cadena, como letras con acentos, la función `strlen()` puede no devolver el número correcto de caracteres. Esto se debe a que Arduino utiliza el juego de caracteres ASCII por defecto y algunos caracteres no están incluidos.
- Al igual que con otras funciones de cadena, `strlen()` cuenta desde cero. Esto significa que el primer carácter en una cadena tiene un índice de 0, el segundo un índice de 1, y así sucesivamente.
- Si no conoces el tamaño máximo de una cadena, es recomendable utilizar una variable de tipo `int` para almacenar el resultado de `strlen()`, ya que no se puede garantizar que la cadena esté dentro del rango de valores de un tipo `byte` o `char`.

## Ver también

- [Documentación de la función `strlen()` de la biblioteca de cadena de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Tutorial de Programación de Arduino: Manipulación de Strings](https://www.arduino.cc/en/Tutorial/StringLengthTrim)