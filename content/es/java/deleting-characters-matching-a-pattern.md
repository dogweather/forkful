---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Java: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Eliminar caracteres que coincidan con un patrón es un proceso común en la programación en Java. Esto implica buscar y eliminar caracteres específicos dentro de una cadena de texto. Los programadores lo hacen para eliminar información irrelevante o potencialmente dañina de los datos que están manejando.

## Cómo hacerlo:

```Java
// Sintaxis básica para eliminar caracteres que coinciden con un patrón
String pattern = "a"; // patrón a buscar
String string = "Hola mundo"; // cadena de texto
string = string.replaceAll(pattern, ""); // elimina todas las "a" de la cadena
System.out.println(string); // resultado: Hol mundo
```

```Java
// Ejemplo de eliminación de caracteres específicos
String pattern = "[aeiou]"; // patrón que coincide con todas las vocales
String string = "Las vacaciones han terminado"; // cadena de texto
string = string.replaceAll(pattern, ""); // elimina todas las vocales de la cadena
System.out.println(string); // resultado: Ls vccns hn trmn

```

## Profundizando:

- Contexto histórico: El proceso de eliminar caracteres que coinciden con un patrón ha sido utilizado en lenguajes de programación desde hace décadas. En Java, el método "replaceAll()" fue introducido en la versión 1.4.

- Alternativas: Además del método "replaceAll()", los programadores pueden utilizar otros métodos como "replace()", que reemplaza todas las apariciones de un carácter específico por otro carácter. También pueden utilizar expresiones regulares para buscar y eliminar patrones más complejos.

- Detalles de implementación: El método "replaceAll()" busca y reemplaza todas las ocurrencias del patrón en la cadena de texto. Si se desea buscar y reemplazar solo la primera ocurrencia, se puede utilizar el método "replaceFirst()".

## Vea también:

- Documentación oficial de Oracle sobre el método "replaceAll()": https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-

- Tutorial de programación Java sobre expresiones regulares: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html