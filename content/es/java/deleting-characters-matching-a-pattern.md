---
title:                "Java: Eliminando caracteres que coinciden con un patrón."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, mientras trabajamos en un proyecto de programación, nos encontramos con la necesidad de eliminar ciertos caracteres de una cadena que coinciden con un patrón específico. Ya sea para formatear datos o para filtrar resultados, la eliminación de caracteres que coinciden con un patrón puede ser una tarea muy útil y necesaria. A continuación, aprenderemos cómo realizar esta tarea en Java y ahorrar tiempo en nuestro trabajo.

## Cómo hacerlo

En Java, podemos utilizar la clase String para realizar operaciones en cadenas de texto, incluyendo la eliminación de caracteres que coinciden con un patrón. La clase String cuenta con varios métodos que nos permiten hacer esta tarea de manera sencilla y eficiente. Veamos un ejemplo:

```java
String cadena = "Esto es un ejemplo con números123 y caracteres especiales!@#$";
cadena = cadena.replaceAll("[0-9!@#$]", "");
System.out.println(cadena);
```
Output: Esto es un ejemplo con números y caracteres especiales

En este ejemplo, utilizamos el método `replaceAll()` de la clase String para eliminar todos los caracteres numéricos y especiales de la cadena original. Utilizamos un patrón entre corchetes, que indica todos los caracteres que queremos eliminar. En este caso, el patrón "[0-9!@#$]" significa que queremos eliminar cualquier dígito o los caracteres especiales !, @, # y $.

Este método es muy versátil, ya que podemos utilizar diferentes patrones según nuestras necesidades. Por ejemplo, si solo queremos eliminar los dígitos de una cadena, podemos utilizar "replaceAll("[0-9]", "")".

Otra opción es utilizar el método `replace()` de la clase String. Este método es más adecuado para eliminar un solo caracter o una secuencia de caracteres específica. Veamos un ejemplo:

```java
String cadena = "¡Ejemplo con guiones-altos---!";
cadena = cadena.replace("-", "");
System.out.println(cadena);
```
Output: ¡Ejemplo con guionesaltos!

En este caso, utilizamos el método `replace()` para eliminar los guiones altos de la cadena original. También podemos utilizar este método para reemplazar los caracteres por otros diferentes, por ejemplo, "replace("!", "?")" reemplazaría todos los signos de exclamación por signos de interrogación.

## Profundizando

Si queremos ser más precisos en nuestra eliminación de caracteres, podemos utilizar expresiones regulares en lugar de patrones específicos. Las expresiones regulares nos permiten especificar un patrón más complejo y detallado para encontrar y eliminar caracteres de una cadena. También tienen una sintaxis diferente a la de los patrones en Java, pero son muy potentes.

Para utilizar expresiones regulares en Java, debemos primero crear un objeto de la clase Pattern y luego utilizar su método `matcher()` para crear un objeto Matcher que nos permitirá buscar el patrón en la cadena. Veamos un ejemplo:

```java
String cadena = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
Pattern patron = Pattern.compile("([aeiou])");
Matcher matcher = patron.matcher(cadena);
cadena = matcher.replaceAll("");
System.out.println(cadena);
```
Output: Lrm psm dlr st mt, cnscttr dpscng lt.

En este ejemplo, utilizamos una expresión regular para eliminar todas las vocales de la cadena original. El patrón "[aeiou]" significa que queremos buscar todas las vocales (mayúsculas y minúsculas) en la cadena. Luego, utilizamos el método `replaceAll()` para reemplazar todas las coincidencias con una cadena vacía.

## Ver también

¡Ahora ya sabes cómo eliminar caracteres que coinciden con un patrón en Java! Si quieres aprender más sobre la clase String y sus métodos, puedes revisar la documentación oficial de Java.

También puedes explorar más sobre expresiones regulares en Java en los siguientes enlaces:

- [Tutorial express regulares en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Documentación oficial de las clases Pattern y Matcher](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Ejemplos prácticos con expresiones regulares en Java](https://www.javacodeexamples.com/java-regex-tutorial/)

¡Sigue practicando y verás lo útiles que pueden ser estas herramientas en tu trabajo diario!