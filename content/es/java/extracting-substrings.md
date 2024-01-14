---
title:                "Java: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer Subcadenas en Java

Extraer subcadenas en Java es una técnica útil para manipular texto y obtener solo la parte deseada de una cadena de caracteres. Con esta habilidad, los programadores pueden crear aplicaciones capaces de procesar datos de manera más eficiente y precisa.

## Cómo hacerlo

Para extraer una subcadena en Java, se utiliza el método `substring()` de la clase `String`. Este método toma dos parámetros: el índice de inicio y el índice de fin de la subcadena deseada. Aquí hay un ejemplo de código que extrae una subcadena de una cadena:

```Java
String cadena = "Hola Mundo";
String subcadena = cadena.substring(5, 10);
System.out.println(subcadena); // Salida: Mundo
```

En este ejemplo, el primer parámetro del método `substring()` es el índice 5, lo que significa que la subcadena comienza en la letra "M". El segundo parámetro es el índice 10, lo que indica que la subcadena termina después de la letra "o". Al imprimir la subcadena, solo se mostrará la palabra "Mundo".

También es posible omitir el segundo parámetro del método `substring()` para extraer una subcadena desde un punto específico hasta el final de la cadena:

```Java
String cadena = "Hola Mundo";
String subcadena = cadena.substring(5);
System.out.println(subcadena); // Salida: Mundo
```

En este caso, la subcadena comenzará en el índice 5 (letra "M") y continuará hasta el final de la cadena.

Otra forma de extraer subcadenas en Java es utilizando el método `split()`, que divide una cadena en una matriz de subcadenas basadas en un delimitador. Por ejemplo:

```Java
String cadena = "Hola-Mundo";
String[] subcadenas = cadena.split("-");
System.out.println(subcadenas[1]); // Salida: Mundo
```

En este ejemplo, la cadena se divide en dos subcadenas: "Hola" y "Mundo", y se almacenan en un arreglo. Al imprimir la subcadena en el índice 1, se mostrará la palabra "Mundo".

## Profundizando

Además de los métodos mencionados anteriormente, hay varias formas de extraer subcadenas en Java, como utilizando expresiones regulares o utilizando las clases `StringTokenizer` o `Matcher`. Sin embargo, es importante tener en cuenta que la extracción de subcadenas puede ser costosa en términos de rendimiento, especialmente cuando se realizan en grandes cadenas. Por lo tanto, es importante evaluar la necesidad real de extraer subcadenas y considerar otras opciones si es posible.

## Ver también

- [Documentación de Java para el método substring](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Tutorial de W3Schools sobre la función `split()` en Java](https://www.w3schools.com/java/ref_string_split.asp)
- [Artículo de StackOverflow sobre el rendimiento de la extracción de subcadenas en Java](https://stackoverflow.com/questions/297230/can-i-improve-java-regular-expression-performance-by-compiling-exceptions-do)