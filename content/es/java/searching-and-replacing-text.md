---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
La búsqueda y reemplazo de texto hace referencia al proceso de encontrar ciertas palabras o caracteres en un texto y cambiarlos por otros. Para los programadores, esto es fundamental cuando se buscan errores o cuando se deben actualizar partes especificas en el código.

## Cómo hacer:

Aquí tenemos un ejemplo de cómo buscar y reemplazar texto en Java.

```java
public class Main {
    public static void main(String[] args) {
        String texto = "Hola Mundo, Hola!";
        String nuevoTexto = texto.replace("Hola", "Adios");
        System.out.println(nuevoTexto);
    }
}
```
La salida de este código sería:

```java
Adios Mundo, Adios!
```
En este ejemplo, cada aparición de la palabra "Hola" se reemplaza por "Adios".

## Inmersión Profunda:

1) En el contexto histórico, la funcionalidad de búsqueda y reemplazo ha sido parte de los lenguajes de programación desde sus inicios. Se considera una herramienta esencial en la mayoría de las aplicaciones de edición de texto.

2) En cuanto a alternativas, la Biblioteca Apache Commons contiene la clase StringUtils, la cual proporciona métodos `replaceEachRepeatedly` y `replaceEach` que permiten reemplazar múltiples subcadenas al mismo tiempo.

3) Acerca de los detalles de implementación, el método `replace` en Java utiliza la técnica de "desplazamiento hacia atrás". Funciona buscando la secuencia objetivo desde el final hacia el principio del texto. Esto ayuda a mantener las iteraciones al mínimo mientras se realiza la sustitución de texto.

## Ver También:

1. [Documentación de Java String replace](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#replace(java.lang.CharSequence,java.lang.CharSequence)): Proporciona información detallada sobre cómo funciona el método `replace` en Java.

2. [Biblioteca Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html): Aquí puedes ver los métodos `replaceEachRepeatedly` y `replaceEach`, útiles para reemplazar varias subcadenas al mismo tiempo.