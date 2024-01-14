---
title:    "Java: Borrando caracteres coincidentes con un patrón"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coincidan con un patrón?

A veces, cuando trabajamos con cadenas de texto en Java, es posible que necesitemos eliminar ciertos caracteres que coincidan con un patrón específico. Esto puede ser útil en situaciones como la validación de entradas de usuario o la limpieza de datos para su posterior procesamiento. En esta publicación de blog, aprenderemos cómo borrar caracteres que coincidan con un patrón usando Java.

## Cómo hacerlo

Usar el método `replaceAll()` es la forma más sencilla de eliminar caracteres que coincidan con un patrón en Java. Este método toma dos argumentos: el patrón que se desea buscar y el texto de reemplazo. A continuación, se muestra un ejemplo de cómo usar este método para eliminar todas las vocales de una cadena:

```java
String texto = "Programación en Java";
String textoSinVocales = texto.replaceAll("[aeiou]", "");
System.out.println(textoSinVocales);
```

La salida de este código sería "Prgrmcón n Jv".

En el ejemplo anterior, utilizamos un patrón entre corchetes para indicar que queremos reemplazar todas las vocales individuales en la cadena. También podemos utilizar expresiones regulares más complejas para realizar reemplazos más específicos. Por ejemplo, podemos eliminar todas las letras mayúsculas o todos los números de una cadena. A continuación, se muestra otro ejemplo donde reemplazamos todas las letras mayúsculas por un espacio en blanco:

```java
String texto = "HolaMundo123";
String textoLimpio = texto.replaceAll("[A-Z]", " ");
System.out.println(textoLimpio);
```

La salida en este caso sería "ola undo123".

Además del método `replaceAll()`, también se puede utilizar el método `replaceFirst()` para borrar solo la primera aparición del patrón en una cadena.

## Profundizando

En la mayoría de los casos, es probable que utilicemos métodos como `replaceAll()` o `replaceFirst()` para borrar caracteres que coincidan con un patrón específico. Sin embargo, para una solución más eficiente, también podemos utilizar la clase `StringBuffer` o `StringBuilder`. Estas clases proporcionan métodos como `deleteCharAt()` o `delete()` que nos permiten eliminar caracteres en una posición específica. Además, podemos usar el método `indexOf()` para obtener la posición de un caracter que coincida con nuestro patrón y, a partir de ahí, realizar las eliminaciones.

## Ver también

- [Documentación oficial de Java sobre el método `replaceAll()`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- [Tutorial sobre expresiones regulares en Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Documentación oficial de Java sobre la clase `StringBuilder`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/StringBuilder.html)