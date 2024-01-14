---
title:                "Java: Encontrando la longitud de una cadena"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué encontrar la longitud de una cadena en Java?

Encontrar la longitud de una cadena es una tarea común en la programación en Java. Saber cómo hacerlo puede ser útil en muchas situaciones, desde validar la entrada del usuario hasta manipular y procesar datos.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Java, podemos usar el método `length()` de la clase `String`. Este método devuelve el número de caracteres en una cadena, incluyendo espacios y caracteres especiales.

```Java
String str = "¡Hola, mundo!";
int length = str.length();
System.out.println("La longitud de la cadena es: " + length);

// Salida: La longitud de la cadena es: 13
```

También podemos usar el mismo método en una cadena vacía y obtendremos una longitud de cero.

```Java
String emptyStr = "";
int length = emptyStr.length();
System.out.println("La longitud de la cadena vacía es: " + length);

// Salida: La longitud de la cadena vacía es: 0
```

También podemos combinar el método `length()` con otros métodos, como `trim()` para obtener la longitud de una cadena después de quitar los espacios en blanco al principio y al final.

```Java
String str = "   Hola ";
int length = str.trim().length();
System.out.println("La longitud de la cadena sin espacios en blanco es: " + length);

// Salida: La longitud de la cadena sin espacios en blanco es: 4
```

## Profundizando

El método `length()` en realidad devuelve el tamaño en memoria de una cadena, no el número de caracteres. Esto se debe a que Java almacena cadenas de caracteres en una tabla de hash, donde cada carácter tiene un índice y el método `length()` simplemente devuelve el índice del último carácter en la cadena más uno.

También es importante tener en cuenta que el método `length()` devolverá un número diferente si se utiliza en diferentes tipos de datos, como `CharSequence` y `StringBuffer`.

## Vea también

- [Método `length()` de la clase String](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#length())
- [Ejemplo de uso del método `length()`](https://www.geeksforgeeks.org/string-length-method-in-java-with-examples/)