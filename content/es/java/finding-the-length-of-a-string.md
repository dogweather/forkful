---
title:                "Java: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, es común trabajar con cadenas de texto o palabras. Muchas veces, necesitamos saber la longitud de estas cadenas para poder realizar ciertas operaciones. En este post, vamos a explorar cómo encontrar la longitud de una cadena en Java y por qué es importante para un programador.

## Cómo

Para encontrar la longitud de una cadena en Java, podemos utilizar el método `length()` de la clase `String`. Este método devuelve un entero que representa el número de caracteres en la cadena.

Veamos un ejemplo de cómo utilizar este método en un programa Java:

```Java
public class LongitudCadena {
  public static void main(String[] args) {
    // Definimos una cadena
    String cadena = "Hola Mundo";

    // Utilizamos el método length() para encontrar la longitud de la cadena
    int longitud = cadena.length();

    // Imprimimos la longitud de la cadena en la consola
    System.out.println("La longitud de la cadena es: " + longitud);
  }
}
```

El resultado de este programa sería:

```
La longitud de la cadena es: 10
```

Como podemos ver, el método `length()` nos devuelve el número de caracteres en la cadena, incluyendo los espacios.

## Deep Dive

En Java, las cadenas de texto se almacenan como un array de caracteres en la memoria. El método `length()` simplemente devuelve la longitud de este array, que coincide con la longitud de la cadena.

Es importante tener en cuenta que el método `length()` cuenta el número de caracteres en la cadena, no el número de palabras. Por ejemplo, si tenemos la cadena "Hola a todos", el resultado de `length()` sería 13, ya que cuenta los espacios también.

También podemos utilizar el método `length()` junto con otros métodos de la clase `String` para realizar diferentes operaciones, como por ejemplo, obtener una subcadena de una cadena dada.

## Ver También

- [Documentación oficial de Java: String.length()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Tutorialspoint: Java - Strings](https://www.tutorialspoint.com/java/java_strings.htm)
- [GeeksforGeeks: Java String Length() method with examples](https://www.geeksforgeeks.org/java-string-length-method-example/)

¡Espero que esta guía te haya ayudado a comprender cómo encontrar la longitud de una cadena en Java y por qué es útil para un programador! ¡Sigue practicando y explorando más sobre este lenguaje para mejorar tus habilidades de programación! ¡Hasta la próxima!