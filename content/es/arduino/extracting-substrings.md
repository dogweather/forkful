---
title:    "Arduino: Extrayendo subcadenas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por qué utilizar la extracción de subcadenas en Arduino?

Extraer subcadenas es una herramienta útil para manipular cadenas de texto en Arduino, permitiendo a los programadores acceder a partes específicas de una cadena y utilizar esa información para realizar tareas específicas en su código.

## Cómo hacerlo:

Para extraer una subcadena de una cadena existente en Arduino, se utiliza la función `substring()` seguida de dos parámetros entre paréntesis: el índice de inicio y la longitud deseada de la subcadena.

```
Arduino String = "¡Hola mundo!"
String subcadena = String.substring (1,4);
Serial.print (subcadena);
```
**Salida:** ola

En este ejemplo, la cadena original es "¡Hola mundo!", y se extrae una subcadena que comienza en la posición 1 (segundo carácter) y tiene una longitud de 4 caracteres. La subcadena resultante sería "ola".

## Profundizando:

Al utilizar la función `substring()` en Arduino, es importante tener en cuenta que los índices de las cadenas comienzan en 0, por lo que el primer carácter tiene un índice de 0, el segundo un índice de 1 y así sucesivamente. También es importante recordar que la longitud de la subcadena debe ser menor o igual a la longitud de la cadena original.

Otra opción es utilizar la función `indexOf()` para encontrar la posición de un caracter específico en una cadena y después utilizar la función `substring()` para extraer una subcadena que comienza en esa posición.

```
Arduino String = "Hello world!"
int pos = String.indexOf("w");
String subcadena = String.substring(pos);
Serial.print(subcadena);
```
**Salida:** world!

En este ejemplo, se busca la posición de la letra "w" en la cadena y se utiliza esa posición como el índice de inicio para la función `substring()`, extrayendo la subcadena "world!".

## Ver También:

Para obtener más información sobre cómo manipular cadenas de texto en Arduino, consulta los siguientes recursos:

- [Documentación oficial de Arduino: Uso de cadenas](https://www.arduino.cc/reference/en/language/variables/data-types/stringclass/)
- [Tutoriales de Arduino: Manipulación de cadenas de texto](https://www.arduino.cc/en/Tutorial/StringLengthTrim)