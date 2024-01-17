---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Java: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

¿Qué y por qué?

Convertir una cadena de texto a minúsculas es un proceso común en la programación de Java. Este proceso implica cambiar todas las letras mayúsculas en una cadena de texto a letras minúsculas. Los programadores hacen esto por varias razones, incluyendo la comparación de cadenas de texto de manera más precisa y la facilidad de manipulación de datos.

¿Cómo hacerlo?

Aquí hay dos ejemplos de código en Java para convertir una cadena de texto a minúsculas:

```Java
// Ejemplo 1: Utilizando el método toLowerCase()
String texto = "HOLA";
String textoMinusculas = texto.toLowerCase();
System.out.println(textoMinusculas); // Output: hola

// Ejemplo 2: Utilizando un bucle for
String texto = "HOLA";
char[] array = texto.toCharArray();
for (int i = 0; i < array.length; i++) {
    if (Character.isUpperCase(array[i])) {
        array[i] = Character.toLowerCase(array[i]);
    }
}
String textoMinusculas = new String(array);
System.out.println(textoMinusculas); // Output: hola
```

Profundizando

La conversión de cadenas de texto a minúsculas ha sido una característica incorporada en Java desde su versión inicial. Antes de esto, los programadores tenían que escribir su propio código para realizar esta tarea.

Además de los métodos mencionados anteriormente, también existen otras formas de convertir cadenas de texto a minúsculas en Java, como utilizar la clase StringBuilder o la librería Apache Commons Lang.

Es importante tener en cuenta que la conversión de cadenas de texto a minúsculas depende del idioma en el que se está trabajando. Algunos idiomas tienen letras mayúsculas y minúsculas específicas que no pueden ser convertidas de manera directa.

¡Consulta estos enlaces para obtener más información sobre cómo realizar una conversión de cadenas de texto a minúsculas en Java!

Ver también

https://www.geeksforgeeks.org/converting-all-letters-of-a-string-to-lowercase-in-java/
https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html 
https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html