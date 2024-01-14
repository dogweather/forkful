---
title:    "Java: Encontrar la longitud de una cadena"
keywords: ["Java"]
---

{{< edit_this_page >}}

#¿Por qué encontrar la longitud de una cadena en Java?

En el mundo de la programación Java, a menudo nos encontramos con la necesidad de manipular cadenas de caracteres. Y una de las operaciones más básicas y comunes es encontrar la longitud de una cadena. Esto puede ser útil para diversas tareas, desde validar la entrada del usuario hasta la manipulación de datos en una base de datos. Por lo tanto, es importante entender cómo encontrar la longitud de una cadena en Java.

##Cómo hacerlo

Para encontrar la longitud de una cadena en Java, podemos utilizar el método `length()` de la clase `String`. Este método devuelve el número de caracteres en una cadena, incluyendo espacios y puntuación.

```Java
String cadena = "¡Hola mundo!";
int longitud = cadena.length();
System.out.println("La longitud de la cadena es: " + longitud);
// Output: La longitud de la cadena es: 12
```

También podemos usar este método en una cadena vacía o nula:

```Java
String cadenaVacia = "";
String cadenaNula = null;
int longitudVacia = cadenaVacia.length();
int longitudNula = cadenaNula.length();
System.out.println("La longitud de la cadena vacía es: " + longitudVacia);
System.out.println("La longitud de la cadena nula es: " + longitudNula);
// Output: La longitud de la cadena vacía es: 0
// Output: Exception in thread "main" java.lang.NullPointerException
```

Es importante tener en cuenta que si intentamos usar el método `length()` en una cadena nula, se producirá una excepción `NullPointerException`. Por lo tanto, siempre debemos asegurarnos de que la cadena no sea nula antes de usar este método.

##Profundizando

Detrás de escena, el método `length()` de la clase `String` utiliza un campo llamado `count` que contiene el número de caracteres en la cadena. También utiliza un algoritmo de búsqueda rápida para encontrar la longitud en O(1) en lugar de O(n) como lo haría un bucle tradicional. Esto hace que el método sea muy eficiente y rápido, especialmente en cadenas largas.

Otra forma de encontrar la longitud de una cadena en Java es utilizando la propiedad `length` de la cadena en lugar del método `length()`. Esta propiedad también devuelve el número de caracteres en una cadena, pero como se trata de una propiedad y no de un método, no necesitamos utilizar los paréntesis después del nombre.

```Java
String cadena = "¡Hola mundo!";
int longitud = cadena.length;
System.out.println("La longitud de la cadena es: " + longitud);
// Output: La longitud de la cadena es: 12
```

##Vea también

- [Documentación oficial de Java sobre el método `length()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Cómo encontrar la longitud de una cadena en Java](https://www.geeksforgeeks.org/length-vs-length-java/)
- [Video tutorial sobre cómo encontrar la longitud de una cadena en Java](https://youtu.be/FiK2StTdjT4)