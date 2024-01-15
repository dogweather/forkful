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

## ¿Por qué?

A veces, al trabajar con cadenas de texto en un programa, es necesario convertir todas las letras a minúsculas. Esto se puede lograr utilizando el método `toLowerCase()` en Java, lo que facilita la comparación de cadenas y la búsqueda de subcadenas.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Java, se puede usar el siguiente código:

```java
String cadena = "¡Hola Mundo!";
String cadenaMin = cadena.toLowerCase();
System.out.println(cadenaMin);
```

El código anterior imprimirá "¡hola mundo!" en la consola, ya que todas las letras de la cadena original se han convertido a minúsculas. 

También se puede convertir una cadena a minúsculas utilizando la clase `StringBuilder`. En este caso, se puede utilizar el método `append()` para agregar cada carácter de la cadena original a un `StringBuilder` y luego usar el método `toLowerCase()` para convertir todas las letras a minúsculas. Por ejemplo:

```java
String cadena = "¡Hola Mundo!";
StringBuilder builder = new StringBuilder();
for (int i = 0; i < cadena.length(); i++) {
    char c = cadena.charAt(i);
    builder.append(Character.toLowerCase(c));
}
String cadenaMin = builder.toString();
System.out.println(cadenaMin);
```

Este código también imprimirá "¡hola mundo!" en la consola.

## Profundizando

Es importante tener en cuenta que el método `toLowerCase()` solo convierte las letras a minúsculas en inglés. Si se desea trabajar con otros idiomas, es necesario utilizar otros métodos como `toUpperCase()` o puede ser necesario utilizar bibliotecas externas para una conversión de texto más precisa.

También es importante mencionar que el método `toLowerCase()` no modifica la cadena original, sino que devuelve una nueva cadena con las letras convertidas a minúsculas. Por lo tanto, si se desea utilizar la cadena en minúsculas, es necesario almacenarla en una nueva variable o reemplazar la cadena original con la cadena en minúsculas.

## Ver también
- [Método toLowerCase() en la documentación de Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Cómo convertir una cadena a mayúsculas en Java](https://www.thoughtco.com/using-touppercase-in-java-tutorial-2034272) (artículo en inglés)
- [Ejemplos de métodos de cadenas en Java](https://beginnersbook.com/2013/12/java-strings/) (artículo en inglés)