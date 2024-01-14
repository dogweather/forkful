---
title:    "Java: Capitalización de una cadena"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

¡Hola lectores de Java!

En esta publicación de blog, vamos a hablar sobre cómo capitalizar una cadena (string en inglés) en Java. Esto puede ser una tarea común en la programación, por lo que es importante conocer cómo hacerlo de manera efectiva. Discutiremos por qué es importante capitalizar una cadena, cómo hacerlo y también profundizaremos en más detalles sobre este tema.

## ¿Por qué?

La capitalización de una cadena es importante en la programación por varias razones. En primer lugar, facilita la legibilidad del código para otros programadores. Al capitalizar correctamente las cadenas, el código es más fácil de entender y seguir. Además, muchas aplicaciones y sistemas tienen reglas estrictas sobre la capitalización de ciertas palabras o nombres, por lo que es importante saber cómo hacerlo correctamente.

## ¿Cómo?

Para capitalizar una cadena en Java, podemos utilizar el método `toUpperCase()` de la clase String. Este método devuelve una nueva cadena con todos los caracteres en mayúsculas. Veamos un ejemplo de código:

```Java
String cadena = "hola mundo";
String cadenaCapitalizada = cadena.toUpperCase();
System.out.println(cadenaCapitalizada);
```
La salida sería: `HOLA MUNDO`

También podemos utilizar el método `replaceFirst()` para capitalizar solo la primera letra de una cadena. Veamos otro ejemplo:

```Java
String cadena = "hola mundo";
String primeraLetra = cadena.substring(0, 1);
String cadenaCapitalizada = primeraLetra.toUpperCase() + cadena.substring(1);
System.out.println(cadenaCapitalizada);
```
La salida sería: `Hola mundo`

También es importante tener en cuenta que Java es sensible a los acentos y mayúsculas. Por lo tanto, si tenemos una cadena con mayúsculas acentuadas, el método `toUpperCase()` no funcionará correctamente. Para evitar esto, podemos utilizar la clase `Locale` y su método `toLanguageTag()` para especificar un idioma específico en el que queremos que se capitalice la cadena.

## Profundizando

Como mencionamos anteriormente, Java es sensible a los acentos y mayúsculas. Esto significa que los métodos `toUpperCase()` y `toLowerCase()` utilizan las reglas del idioma inglés. Sin embargo, para aplicaciones multilingües, esto puede ser un problema. Para solucionarlo, tenemos la clase `Locale`, que nos permite especificar un idioma específico para aplicar las reglas de capitalización adecuadas. Veamos un ejemplo:

```Java
String cadena = "bonjour le monde";
String cadenaCapitalizada = cadena.toUpperCase(new Locale("fr"));
System.out.println(cadenaCapitalizada);
```
La salida sería: `BONJOUR LE MONDE`

Aquí, al especificar el idioma francés en el método `toUpperCase()`, las reglas de capitalización francesas se aplican a la cadena, en lugar de las reglas en inglés.

## Ver también

- [Documentación oficial de Java sobre el método `toUpperCase()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Métodos de la clase String en Java](https://www.geeksforgeeks.org/string-methods-java/)
- [Tutorial de Java: Aprende a programar en Java desde cero](https://www.aprenderaprogramar.net/tutoriales/?seccion=java)

¡Esperamos que esta publicación te haya sido útil para aprender a capitalizar cadenas en Java! ¡Hasta la próxima!