---
title:                "Encontrando la longitud de una cadena"
html_title:           "Java: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez te has preguntado cómo se puede saber la longitud de una cadena de texto en Java? La respuesta es bastante sencilla y en este artículo te enseñaremos cómo hacerlo.

## Cómo hacerlo

Para encontrar la longitud de una cadena de texto en Java, utilizamos el método `length()` del objeto `String`. Aquí tienes un ejemplo de código para que lo puedas probar:

```Java
String cadena = "Hola mundo";
System.out.println(cadena.length());
```

La salida de este código será `10` ya que la cadena "Hola mundo" contiene 10 caracteres, incluyendo el espacio en blanco. También puedes usar este método en cadenas vacías, en cuyo caso la salida será `0`.

```Java
String cadenaVacia = "";
System.out.println(cadenaVacia.length());
```

La salida de este código será `0`.

## Profundizando

El método `length()` pertenece a la clase `String`, que es una clase de la biblioteca estándar de Java que se utiliza para almacenar y manipular cadenas de texto. Este método nos permite obtener la longitud de una cadena de texto, es decir, el número de caracteres que contiene.

Es importante mencionar que `length()` devuelve un valor entero de tipo `int`, lo que significa que no podemos utilizarlo para cadenas muy largas que superen el límite de este tipo de datos.

También hay que tener en cuenta que `length()` cuenta los caracteres de una cadena incluyendo los espacios en blanco, por lo que si queremos saber el número de letras en una palabra, debemos restar el espacio en blanco de la cuenta.

## Vea también

- Documentación oficial de Java sobre `String`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Tutorial de Java sobre cómo contar caracteres en una cadena: https://www.javatpoint.com/how-to-count-characters-in-string-in-java