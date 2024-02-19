---
aliases:
- /es/java/extracting-substrings/
date: 2024-01-20 17:45:49.623457-07:00
description: "Extraer subcadenas significa seleccionar partes espec\xEDficas de una\
  \ cadena de texto. Programadores lo hacen para analizar, manipular o transformar\
  \ datos de\u2026"
lastmod: 2024-02-18 23:09:09.833320
model: gpt-4-1106-preview
summary: "Extraer subcadenas significa seleccionar partes espec\xEDficas de una cadena\
  \ de texto. Programadores lo hacen para analizar, manipular o transformar datos\
  \ de\u2026"
title: "Extracci\xF3n de subcadenas"
---

{{< edit_this_page >}}

## What & Why? (¿Qué y Por Qué?)
Extraer subcadenas significa seleccionar partes específicas de una cadena de texto. Programadores lo hacen para analizar, manipular o transformar datos de manera más eficiente.

## How to:
Para extraer subcadenas en Java usamos el método `substring()`. Ejemplos:

```java
public class Main {
    public static void main(String[] args) {
        String message = "Hola, Mundo!";
        
        // Extraer desde el carácter en la posición 5 (inclusive) hasta el final
        String sub = message.substring(5);
        System.out.println(sub); // "Mundo!"
        
        // Extraer desde la posición 5 (inclusive) hasta la 7 (exclusive)
        String smallSub = message.substring(5, 7);
        System.out.println(smallSub); // "Mu"
    }
}
```

Output:
```
Mundo!
Mu
```

## Deep Dive (Inmersión Profunda):
La capacidad de extraer subcadenas es antigua como los propios lenguajes de programación. En Java, el método `substring(int beginIndex, int endIndex)` ha sido parte del `String` desde JDK 1.0. Antes de Java 7, cuando se ejecutaba `substring()`, se compartía el array de caracteres original para economizar memoria, pero eso cambió debido a problemas de fuga de memoria. Ahora, crea una nueva cadena, lo cual es más seguro, aunque un poco menos eficiente en memoria.

Alternativas a `substring()` incluyen métodos como `split()`, `StringTokenizer` o expresiones regulares con `Pattern` y `Matcher` para casos más complejos. Cada uno tiene su ventaja dependiendo del contexto.

Detalles de implementación: Internamente, `substring()` utiliza el método `Arrays.copyOfRange()` para copiar el rango relevante de caracteres, lo que garantiza que la nueva cadena sea independiente de la original.

## See Also (Ver También):
- Documentación oficial de Oracle para el método `substring`: [Oracle Docs - String substring](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int))
- Tutorial sobre expresiones regulares en Java: [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
