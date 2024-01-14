---
title:    "Java: Eliminando caracteres que coinciden con un patrón"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en una situación en la que necesitas eliminar ciertos caracteres de una cadena según un patrón determinado? Puede parecer una tarea tediosa, pero en realidad puede ser bastante útil en ciertas situaciones. En esta entrada del blog, aprenderemos cómo eliminar caracteres que coincidan con un patrón en Java.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón, utilizaremos el método `replaceAll()` de la clase `String` en Java. Este método acepta dos parámetros: el primer parámetro es el patrón utilizado para buscar los caracteres que deseamos eliminar y el segundo parámetro es el valor que reemplazará a los caracteres coincidentes.

Veamos un ejemplo de cómo utilizar `replaceAll()` en un programa Java:

```Java
String cadena = "1a2b3c4d5e6";
String reemplazo = "";

String nuevaCadena = cadena.replaceAll("[a-z]", reemplazo);
System.out.println(nuevaCadena);
```

En este ejemplo, hemos creado una cadena con algunos números y letras. Utilizando el patrón `"[a-z]"`, indicamos que queremos reemplazar todas las letras minúsculas con una cadena vacía, es decir, eliminarlas. Al imprimir `nuevaCadena`, obtendremos el resultado: `123456`.

Si solo queremos eliminar caracteres numéricos, podemos utilizar el patrón `"[0-9]"`. También podemos combinar diferentes patrones como `"[^a-zA-Z]"`, que eliminará todos los caracteres que no sean letras.

Ahora veamos un ejemplo más complejo utilizando expresiones regulares para eliminar caracteres no deseados:

```Java
String cadena = "¡H0l4 MµnD0!";
String reemplazo = "";

String nuevaCadena = cadena.replaceAll("[^a-zA-ZáéíóúÁÉÍÓÚ]", reemplazo);
System.out.println(nuevaCadena);
```

Este ejemplo utilizará el patrón `"[^a-zA-ZáéíóúÁÉÍÓÚ]"` para eliminar todos los caracteres que no sean letras y acentos. Al imprimir `nuevaCadena`, obtendremos el resultado: `HlMnD`.

## Profundizando

El método `replaceAll()` utiliza expresiones regulares para buscar y reemplazar caracteres, lo que significa que podemos utilizar diferentes patrones para diferentes situaciones. Por ejemplo, si solo queremos eliminar los espacios en blanco de una cadena, podemos utilizar el patrón `"\\s"`.

También es importante tener en cuenta que el método `replaceAll()` devuelve una nueva cadena, por lo que la cadena original no se ve afectada. Si deseamos modificar la cadena original, podemos asignar el resultado a la misma variable en la que estábamos trabajando.

## Ver también

- [Documentación de Java para el método `replaceAll()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Tutorial de expresiones regulares en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)