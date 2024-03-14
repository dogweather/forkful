---
date: 2024-01-20 17:47:51.574191-07:00
description: "Encontrar la longitud de una cadena significa saber cu\xE1ntos caracteres\
  \ contiene. Programadores lo hacen para validar textos, limitar entradas, iterar\u2026"
lastmod: '2024-03-13T22:44:58.928936-06:00'
model: gpt-4-1106-preview
summary: "Encontrar la longitud de una cadena significa saber cu\xE1ntos caracteres\
  \ contiene. Programadores lo hacen para validar textos, limitar entradas, iterar\u2026"
title: Calculando la longitud de una cadena
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Encontrar la longitud de una cadena significa saber cuántos caracteres contiene. Programadores lo hacen para validar textos, limitar entradas, iterar correctamente y evitar errores comunes.

## Cómo hacerlo:

Para obtener la longitud de una cadena en Java, usas el método `.length()`. Aquí tienes un ejemplo sencillo:

```java
public class Main {
    public static void main(String[] args) {
        String saludo = "¡Hola, Mundo!";
        int longitud = saludo.length();
        System.out.println("La longitud de la cadena es: " + longitud);
    }
}
```

Output:
```
La longitud de la cadena es: 13
```

## Profundizando:

Históricamente, la función de longitud de una cadena es una de las operaciones más básicas en la programación. En Java, `.length()` ha sido parte del lenguaje desde su versión inicial. Hay detalles que debes considerar:

1. **Cuenta de Caracteres Unicode:** Un punto importante es cómo `.length()` cuenta los caracteres Unicode que requieren más de un `char` en Java. `length()` devuelve el número de unidades de código `char`, lo que podría no coincidir con el número real de caracteres si usas caracteres fuera del Plano Multilingüe Básico (BMP).
2. **Alternativas:** Si necesitas trabajar con todos los caracteres Unicode correctamente, podrías usar la clase `codePointCount` para obtener la cuenta real de los caracteres.
3. **Implementación:** La implementación de `.length()` es directa ya que las cadenas en Java se almacenan internamente como arrays de caracteres, y `.length()` simplemente devuelve la longitud de ese array.

## Ver También:

- [Documentación oficial de String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Tutorial de Oracle sobre cadenas](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Artículo sobre UTF-16 y Java strings](https://www.oracle.com/technical-resources/articles/javase/supplementary.html)
