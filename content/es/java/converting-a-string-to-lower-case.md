---
title:                "Java: Convirtiendo una cadena a minúsculas"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

La conversión de una cadena a minúsculas puede ser útil en situaciones en las que se necesite realizar una comparación de cadenas sin importar las mayúsculas o minúsculas. También puede ser útil para formatear correctamente los datos ingresados por un usuario antes de procesarlos.

## Cómo hacerlo

```java
String cadena = "Hola Mundo";
String cadenaMinus = cadena.toLowerCase();
System.out.println(cadenaMinus);
```

Salida: `hola mundo`

Para convertir una cadena a minúsculas en Java, simplemente usamos el método `toLowerCase()` en la cadena que queremos convertir. Este método devuelve una nueva cadena con todos los caracteres convertidos a minúsculas. También se puede utilizar `toUpperCase()` para convertir una cadena a mayúsculas.

## Profundizando

El método `toLowerCase()` utiliza reglas de transformación definidas por el Unicode Consortium para convertir los caracteres a minúsculas. Estas reglas cubren una variedad de idiomas y casos especiales como las letras con acentos o diacríticos. También hay métodos sobrecargados que nos permiten especificar un `Locale` para la conversión, lo que es especialmente útil cuando se trabaja con idiomas que utilizan caracteres especiales como el alemán.

## Ver también

- [Documentación oficial de Java sobre String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial sobre el uso de métodos de Strings en Java](https://www.w3schools.com/java/java_ref_string.asp)
- [Ejemplos prácticos de conversión de cadenas en Java](https://www.baeldung.com/java-string-tolowercase)