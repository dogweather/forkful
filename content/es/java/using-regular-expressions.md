---
title:                "Utilizando expresiones regulares"
html_title:           "Java: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Las expresiones regulares son secuencias de caracteres que se utilizan para buscar y manipular patrones en un texto. Los programadores utilizan expresiones regulares para realizar tareas como validar entradas de usuario y buscar información en grandes cantidades de datos.

## ¿Cómo hacerlo?
Para utilizar expresiones regulares en Java, necesitas importar la clase java.util.regex. Luego, puedes utilizar sus métodos para buscar patrones en una cadena de texto. Por ejemplo:
```Java
import java.util.regex.*;

String codigoPostal = "12345";
boolean resultado = Pattern.matches("\\d{5}", codigoPostal);
System.out.println(resultado);
```
Este código verifica si la variable "codigoPostal" es un código postal válido de 5 dígitos. En este caso, el resultado sería "true".

## Profundizando
Las expresiones regulares tienen su origen en la teoría computacional de los años 50 y fueron adoptadas por primera vez en Unix en los años 70. Actualmente, existen otros métodos para manipular patrones en texto, como las funciones de búsqueda y reemplazo, pero las expresiones regulares siguen siendo una herramienta importante para los programadores.

## Ver También
- [Documentación de java.util.regex](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Tutorial de expresiones regulares de Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)