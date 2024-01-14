---
title:                "Java: Extracción de subcadenas"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué
Extraer subcadenas es una habilidad importante en la programación de Java. Nos permite obtener una parte específica de una cadena de texto, lo que puede ser útil en una amplia gama de aplicaciones. En esta publicación, aprenderemos por qué y cómo usar esta técnica en nuestros programas.

## Cómo hacerlo
Para extraer subcadenas en Java, utilizaremos el método `substring()` que viene incluido en la clase `String`. Este método toma dos parámetros: el índice inicial e índice final de la subcadena que queremos extraer. Veamos un ejemplo:

```Java
String original = "Hola mundo!";
String subcadena = original.substring(0, 4);
System.out.println(subcadena);
```

En este caso, hemos extraído los primeros cuatro caracteres de la cadena original y los hemos asignado a la variable `subcadena`. Al imprimir `subcadena`, obtendremos como resultado "Hola".

Si solo queremos especificar el índice inicial y obtener la subcadena hasta el final, podemos omitir el segundo parámetro. Por ejemplo:

```Java
String original = "Hola mundo!";
String subcadena = original.substring(5);
System.out.println(subcadena);
```

En este caso, hemos extraído la subcadena "mundo!" y la hemos asignado a la variable `subcadena`.

También podemos especificar índices negativos para obtener subcadenas a partir del final de la cadena. Por ejemplo:

```Java
String original = "Hola mundo!";
String subcadena = original.substring(-5, -1);
System.out.println(subcadena);
```

En este caso, hemos extraído los últimos cuatro caracteres de la cadena original y obtendremos "undo" como resultado.

## Profundizando
Además de los ejemplos mencionados anteriormente, hay algunas cosas importantes a tener en cuenta al trabajar con `substring()`.

- El índice inicial debe ser mayor o igual a cero y menor que el tamaño de la cadena original.
- El índice final debe ser mayor que el índice inicial y menor o igual al tamaño de la cadena original.
- Si omitimos el segundo parámetro, el método asume que queremos extraer la subcadena desde el índice inicial hasta el final de la cadena.
- Si especificamos un índice negativo, Java tomará el valor absoluto de ese índice y lo usará como si fuera positivo.
- El método `substring()` no modifica la cadena original, sino que devuelve una nueva cadena con los caracteres extraídos.

Teniendo en cuenta estas consideraciones, podemos utilizar el método `substring()` de manera efectiva en nuestros programas Java para extraer subcadenas según nuestras necesidades.

## Ver también
- [Documentación oficial de Java sobre el método `substring()`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int)).
- [Artículo de Programiz con más ejemplos de uso de `substring()`](https://www.programiz.com/java-programming/library/string/substring).
- [Página de tutorialspoint sobre cómo extraer subcadenas en Java](https://www.tutorialspoint.com/java/java_string_substring.htm).