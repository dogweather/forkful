---
title:                "Capitalizando una cadena"
html_title:           "Java: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si eres un programador Java, probablemente te has encontrado con la tarea de capitalizar una cadena de texto. La razón más común para hacer esto es formatear una cadena de nombres o títulos en un formato más legible.

## ¿Cómo?

Para capitalizar una cadena de texto en Java, podemos utilizar el método `toUpperCase()` o `toTitleCase()` de la clase `String`. Ambos métodos convierten la primera letra de cada palabra en mayúscula.

```Java
String cadena = "hola, soy un programador Java";
System.out.println(cadena.toUpperCase()); // Salida: HOLA, SOY UN PROGRAMADOR JAVA

System.out.println(cadena.toTitleCase()); // Salida: Hola, Soy Un Programador Java
```

Otra alternativa es utilizar la clase `Character` y su método `toUpperCase()` para convertir únicamente la primera letra de la cadena a mayúscula.

```Java
String cadena = "hola, soy un programador Java";
String primeraLetra = Character.toUpperCase(cadena.charAt(0)) + cadena.substring(1);
System.out.println(primeraLetra); // Salida: Hola, soy un programador Java
```

## Profundizando

Es importante tener en cuenta que estos métodos solo convierten la primera letra de cada palabra, por lo que si tenemos nombres o títulos con apóstrofes, estos no serán capitalizados. Además, si la cadena ya contiene letras en mayúscula, estas no serán afectadas por los métodos mencionados anteriormente.

Para resolver estos problemas, podemos utilizar la clase `WordUtils` de la biblioteca Apache Commons Lang, que tiene el método `capitalizeFully()` que capitaliza todas las palabras de una cadena, incluyendo aquellas con apóstrofes y deja intactas las letras que ya están en mayúscula.

```Java
String cadena = "McDonald's es una cadena de restaurantes";
System.out.println(WordUtils.capitalizeFully(cadena); // Salida: McDonald's Es Una Cadena De Restaurantes
```

## Ver también

- [Método `toUpperCase()` de la clase `String` en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Método `toTitleCase()` de la clase `String` en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toTitleCase-int-) 
- [Método `toUpperCase()` de la clase `Character` en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toUpperCase-char-) 
- [Clase `WordUtils` de Apache Commons Lang](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html)