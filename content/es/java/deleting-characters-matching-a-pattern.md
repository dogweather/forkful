---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es una acción común en programación. Esto significa identificar y eliminar todos los caracteres que encajan en un patrón determinado, usualmente para simplificar y limpiar nuestros datos.

## Cómo hacerlo:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Prueba123";
        System.out.println(str.replaceAll("[0-9]", ""));
    }
}
```
Salida:
```
Prueba
```
En este ejemplo, hemos eliminado todos los números (0-9) de nuestro string, dejando únicamente las letras.

## Análisis en Profundidad:

Este método ha existido desde las primeras versiones de Java, pero se ha refinado y optimizado a lo largo del tiempo. Una alternativa a `replaceAll()` es utilizar un `StringBuilder` para construir la cadena sin los caracteres indeseados. Sin embargo, `replaceAll()` es más intuitivo y proporciona una solución en una sola línea. La implementación se basa en expresiones regulares, una poderosa herramienta para trabajar con cadenas.

## Ver También:

Para más información sobre la manipulación de cadenas en Java, puedes consultar los siguientes recursos:

- Oracle Java Documentation: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- The Java Tutorials: [Lesson: Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)
- Stack Overflow: [How to remove characters from a string?](https://stackoverflow.com/questions/8751653/how-to-remove-characters-from-a-string)