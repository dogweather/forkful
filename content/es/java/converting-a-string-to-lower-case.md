---
title:    "Java: Convirtiendo una cadena en minúsculas"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

La conversión de una cadena de texto a minúsculas es una tarea común en la programación Java. Esta operación puede ser útil para comparar cadenas de texto de manera más eficiente o para estandarizar la entrada de datos del usuario.

## Cómo hacerlo

La clase String de Java proporciona un método llamado "toLowerCase()" que permite convertir una cadena a minúsculas. Este método devuelve una nueva cadena con todos los caracteres en minúsculas, dejando intacta la cadena original. Veamos un ejemplo:

```Java
String nombre = "JUAN";
String nombreMinuscula = nombre.toLowerCase();
System.out.println(nombreMinuscula); // salida: juan
```

También es posible utilizar el método "toLowerCase(Locale locale)" para especificar el idioma en el que se realizará la conversión. Por ejemplo, si queremos que la cadena se convierta a minúsculas en español, podemos usar la siguiente sintaxis:

```Java
String palabra = "HOLA";
String palabraMinuscula = palabra.toLowerCase(new Locale("es"));
System.out.println(palabraMinuscula); // salida: hola
```

## Profundizando

Es importante tener en cuenta que al convertir una cadena a minúsculas, se tomará en cuenta la configuración regional del sistema en el que se esté ejecutando el programa. Esto significa que si la configuración regional es diferente al español, el resultado de la conversión puede variar.

Además, la conversión a minúsculas no solo aplica a las letras, sino también a todos los caracteres que tengan una equivalente en minúsculas. Por ejemplo, la letra "Á" se convertirá a "á" y la letra "Ü" se convertirá a "ü".

## Ver también

- Documentación oficial de Java para el método "toLowerCase()": https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- Documentación oficial de Java para la clase Locale: https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html
- Ejemplos de conversión a minúsculas en diferentes idiomas: https://www.geeksforgeeks.org/java-string-tolowercase-locale-methods-with-examples/