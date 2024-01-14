---
title:                "Java: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por Qué

En programación, a menudo nos encontramos con la necesidad de manipular cadenas de texto. Una forma común de hacerlo es convirtiendo todas las letras a minúsculas. Esto puede ser útil para comparar cadenas de texto sin importar si están escritas en mayúsculas o minúsculas, o simplemente para mostrar siempre la información en un formato uniforme. A continuación, veremos cómo podemos hacer esto en Java.

## Cómo Hacerlo

Para convertir un String a minúsculas en Java, podemos utilizar el método `toLowerCase()` de la clase String. Este método no modifica el String original, sino que devuelve un nuevo String con todas las letras en minúsculas. Veamos un ejemplo de cómo podemos usarlo:

```Java
String texto = "Hola Mundo";
String textoEnMinusculas = texto.toLowerCase();
System.out.println(textoEnMinusculas); // output: hola mundo
```

Como se puede ver en el ejemplo, el método `toLowerCase()` se puede aplicar directamente sobre una cadena de texto. También podemos almacenar el nuevo String devuelto en una variable, como se hizo en el ejemplo.

También podemos convertir una cadena de texto a minúsculas utilizando la clase `Locale` de Java. Este enfoque puede ser útil si queremos convertir la cadena a minúsculas utilizando las reglas de una localización específica. Veamos otro ejemplo:

```Java
String texto = "HOLA MUNDO";
String textoEnMinusculas = texto.toLowerCase(Locale.forLanguageTag("es-ES"));
System.out.println(textoEnMinusculas); // output: hola mundo
```

En este ejemplo, utilizamos el método `toLowerCase()` pero pasando como parámetro un objeto `Locale` para especificar que queremos utilizar las reglas del idioma español. Esto es especialmente útil si estamos trabajando con contenido multilingüe.

## Profundizando

Además del método `toLowerCase()`, existen otros métodos en Java que nos permiten manipular cadenas de texto y convertirlas a minúsculas. Por ejemplo, el método `toLowerCase()` solo convierte las letras de la cadena a minúsculas, pero si queremos también convertir los acentos y caracteres especiales, podemos utilizar el método `toLowerCase(Locale)` de la clase Normalizer, que nos permite especificar una localización específica.

Otra forma de convertir una cadena de texto a minúsculas es utilizando expresiones regulares. En este caso, podemos utilizar la clase `Pattern` y su método `matcher()` para encontrar todas las letras mayúsculas en la cadena y reemplazarlas por su versión en minúsculas. Si estás interesado en aprender más sobre expresiones regulares, te recomiendo que leas nuestro artículo [Introducción a las Expresiones Regulares en Java](https://www.ejemplo.com/introduccion-expresiones-regulares-java/).

## Ver También

- [Método toLowerCase() de la clase String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Método toLowerCase() de la clase Normalizer](https://docs.oracle.com/javase/8/docs/api/java/text/Normalizer.html#normalize-java.lang.CharSequence-java.text.Normalizer.Form-)