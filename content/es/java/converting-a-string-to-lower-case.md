---
title:    "Java: Convirtiendo una cadena a minúsculas"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué:

Convertir una cadena de caracteres a minúsculas es una habilidad importante en el mundo de la programación Java. A menudo, es necesario manipular y comparar cadenas de caracteres en un formato uniforme, lo que hace que la conversión a minúsculas sea útil para asegurar consistencia en los datos.

## Cómo hacerlo:

En Java, hay varias formas de convertir una cadena de caracteres a minúsculas. Una opción es utilizar el método `toLowerCase()` de la clase `String`. Este método devuelve una nueva cadena de caracteres con todas las letras convertidas a minúsculas.

```
public class ConvertirAMinusculas{
    public static void main(String[] args) {
        String cadena = "ESTE ES UNA CADENA EN MAYÚSCULAS";
        String cadenaMinusculas = cadena.toLowerCase();
        System.out.println(cadenaMinusculas);
    }
}
```

La salida del código anterior sería "este es una cadena en mayúsculas". Este método también es útil para comparar dos cadenas de caracteres de manera no sensible a mayúsculas y minúsculas, utilizando el método `equalsIgnoreCase()` también de la clase `String`.

Otra opción para convertir a minúsculas es utilizar la clase `Locale` y el método `toLowercase()`. Esta clase nos permite especificar un idioma para la conversión, lo que puede ser útil en situaciones en las que se manejan diferentes idiomas o caracteres específicos.

```
public class ConvertirAMinusculas{
    public static void main(String[] args) {
        String cadena = "ESTE ES UNA CADENA EN MAYÚSCULAS";
        String cadenaMinusculas = cadena.toLowerCase(Locale.ENGLISH);
        System.out.println(cadenaMinusculas);
    }
}
```

La salida del código anterior sería la misma que en el primer ejemplo. Sin embargo, si especificamos un idioma diferente, la salida podría ser diferente dependiendo de la estructura y uso de mayúsculas y minúsculas en ese idioma.

## Profundizando:

La conversión de una cadena de caracteres a minúsculas puede parecer un proceso sencillo, pero es importante comprender cómo funcionan los diferentes métodos de conversión y en qué situaciones pueden ser más útiles. También es importante tener en cuenta que la conversión a minúsculas puede verse afectada por diferentes idiomas y caracteres especiales, por lo que es necesario tener cuidado al usarla en estos casos.

## Ver también:

- [Método toLowerCase () de la clase String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Método equalsIgnoreCase () de la clase String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#equalsIgnoreCase(java.lang.String))
- [Clase Locale en Java](https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html)
- [Método toLowerCase () de la clase Locale](https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html#toLowerCase())