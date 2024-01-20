---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Extraer subcadenas es el proceso de obtener una parte de una cadena en Java. Los programadores lo hacen para manipular datos más pequeños y útiles dentro de una cadena más grande.

## Cómo hacerlo

Supongamos que tienes una cadena "¡Hola, mundo de la programación!" y quieres obtener la palabra "mundo". Aquí te explico cómo:

```Java
// Declarar la cadena fuente
String str = "¡Hola, mundo de la programación!";
// Definir los índices de inicio y fin
int start = 7;
int end = 12;
// Extraer la subcadena
String sub = str.substring(start, end);
// Imprimir la subcadena
System.out.print(sub);
```
El código anterior imprimirá:

```
mundo
```
## Inmersión profunda

La función `substring()` ha sido una parte integral de Java desde su primera versión, JDK 1.0. En Java, las cadenas son inmutables, por lo que `substring()` no altera la cadena original, sino que crea una nueva.

Existen alternativas a `substring()`:

- `split()`: Se usa para dividir una cadena en un array basándose en una expresión regular.
- `charAt()`: Obtiene un solo carácter de la cadena. Podrías usar un bucle para obtener múltiples caracteres.

Esto es importante: en Java, el primer carácter de la cadena está en el índice 0. Hay dos versiones del método `substring()`:

- `substring(int beginIndex)`: Devuelve una nueva cadena que comienza desde 'beginIndex' hasta el final de la cadena.
- `substring(int beginIndex, int endIndex)`: Devuelve una nueva cadena que comienza desde 'beginIndex' hasta 'endIndex' (exclusivo).

## Ver También

- [Documentación Oficial de Java - Clase String](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [Tutorial de Java - Cadena de caracteres](https://www.w3schools.com/java/java_strings.asp)
- [Explicación detallada del método substring()](https://www.javatpoint.com/java-string-substring)