---
title:    "Java: Extrayendo subcadenas"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué
Muchos programadores se encuentran en situaciones donde necesitan extraer una sección específica de un texto o cadena de caracteres. Esto puede ser útil al trabajar con grandes cantidades de datos o al procesar información recibida de una fuente externa. Afortunadamente, en Java contamos con métodos para extraer substrings de manera eficiente y precisa.

## Cómo hacerlo
Para extraer un substring en Java, podemos utilizar el método `substring()` de la clase `String`. Este método toma dos parámetros: el índice inicial y el índice final del substring que deseamos extraer. Por ejemplo, si tenemos la siguiente cadena de texto: "Hola mundo", y queremos extraer "mundo", podemos hacerlo de la siguiente manera:

```Java
String texto = "Hola mundo";
String mundo = texto.substring(5, 10);
System.out.println(mundo); // Salida: mundo
```

El primer parámetro del método `substring()` es inclusivo, lo que significa que el carácter en ese índice será parte del substring. Mientras que el segundo parámetro es exclusivo, por lo que el carácter en ese índice no será incluido en el substring resultante.

Además, también podemos utilizar el método `substring()` para extraer un substring a partir de un índice específico hasta el final de la cadena, simplemente omitiendo el segundo parámetro. Por ejemplo:

```Java
String texto = "Hola mundo";
String mundo = texto.substring(5);
System.out.println(mundo); // Salida: mundo
```

Otra forma de extraer substrings es utilizando el método `split()` de la clase `String`. Este método toma una expresión regular como parámetro y divide la cadena en un arreglo de substrings basado en esa expresión. Por ejemplo, si queremos extraer todas las palabras de una frase, podemos hacerlo de esta manera:

```Java
String frase = "Esta es una frase de ejemplo";
String[] palabras = frase.split("\\s+"); // Dividimos la cadena en base a uno o varios espacios en blanco
for (String palabra : palabras) {
    System.out.println(palabra);
}
```

La salida sería:

```
Esta
es
una
frase
de
ejemplo
```

## Profundizando
Los métodos `substring()` y `split()` son solo algunas de las formas en que podemos extraer substrings en Java. También podemos utilizar expresiones regulares más complejas y métodos como `indexOf()` y `lastIndexOf()` para obtener resultados más específicos. Es importante estar familiarizado con estas funcionalidades ya que nos pueden ahorrar mucho tiempo y esfuerzo en el procesamiento de cadenas de caracteres.

## Ver también
- Documentación oficial de la clase String en Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Guía de expresiones regulares en Java: https://docs.oracle.com/javase/tutorial/essential/regex/
- Ejemplos de código para extraer substrings en Java: https://www.javatpoint.com/java-string-substring