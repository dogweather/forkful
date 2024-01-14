---
title:                "Java: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena

En la programación Java, a menudo nos encontramos con la necesidad de capitalizar una cadena de texto para que la primera letra de cada palabra esté en mayúscula. Esto es especialmente útil cuando se trabaja con entradas de usuarios que pueden estar en minúsculas o cuando se desea seguir una convención de nomenclatura específica.

## Cómo hacerlo

Para capitalizar una cadena en Java, se puede utilizar el método `toUpperCase()` de la clase `String`. Este método convierte todas las letras de una cadena en mayúsculas. Sin embargo, para capitalizar solo la primera letra de cada palabra, podemos usar el siguiente código:

```Java
String input = "hola mundo";
StringBuilder output = new StringBuilder();

// Divide la cadena en palabras
String[] words = input.split(" ");
for (String word : words) {
    // Convierte la primera letra en mayúscula y concatena con el resto de la palabra en minúscula
    String capitalizedWord = word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase();
    // Agrega la palabra capitalizada al string de salida
    output.append(capitalizedWord).append(" ");
}

// Imprime el resultado final
System.out.println(output); // Salida: Hola Mundo
```

Este código divide la cadena en palabras utilizando el espacio como separador, y luego itera sobre cada palabra para capitalizar la primera letra y convertir el resto en minúsculas. Finalmente, se imprime el resultado en la consola.

## Profundizando

En el código anterior, utilizamos el método `split()` de la clase `String` para dividir la cadena en palabras. Este método también acepta un patrón regular como argumento, lo que lo hace más flexible para casos en los que los separadores pueden variar. Por ejemplo, si las palabras estuvieran separadas por espacios, comas y guiones, podríamos usarlo de la siguiente manera:

```Java
String input = "hola, java-mundo";
String[] words = input.split("[ ,\\-]");
```

Además, podemos agregar una validación para manejar casos en los que la cadena esté vacía o no contenga ninguna palabra.

## Ver también

- [Java String class API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Regular Expressions in Java](https://www.baeldung.com/java-regexp)
- [Validación de entradas de usuarios en Java](https://www.geeksforgeeks.org/how-to-validate-an-input-using-javafx/)