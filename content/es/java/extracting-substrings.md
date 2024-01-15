---
title:                "Extrayendo subcadenas"
html_title:           "Java: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad útil para cualquier programador Java. Puedes utilizarla para manipular cadenas de texto de manera eficiente, realizar búsquedas o comparaciones de cadenas, y realizar transformaciones en tus datos. En resumen, saber cómo extraer subcadenas te ayudará a ser un programador más eficiente y productivo.

## Cómo hacerlo

Para extraer una subcadena de una cadena en Java, puedes utilizar el método `substring()` que está presente en la clase `String`. Este método toma dos parámetros: el índice inicial y el índice final de la subcadena que deseas extraer. Aquí tienes un ejemplo de código:

```Java
String cadena = "Hola a todos!";
String subcadena = cadena.substring(5, 10);
System.out.println(subcadena); // Output: a todo
```

En el ejemplo anterior, utilizamos el método `substring()` para extraer la subcadena "a todo" de la cadena original. El primer parámetro indica el índice a partir del cual se inicia la extracción, y el segundo parámetro indica el índice justo antes del cual se detiene la extracción. Ten en cuenta que los índices en Java empiezan desde 0, por lo tanto, el primer carácter tiene índice 0.

También puedes utilizar el método `substring()` con un solo parámetro, que indica el índice a partir del cual se extrae hasta el final de la cadena. Aquí tienes un ejemplo:

```Java
String cadena = "Hola a todos!";
String subcadena = cadena.substring(5);
System.out.println(subcadena); // Output: a todos!
```

Este ejemplo extrae la subcadena "a todos!" de la cadena original, empezando en el índice 5 y extrayendo hasta el final de la cadena.

Ten en cuenta que el método `substring()` no modifica la cadena original, sino que devuelve una nueva cadena como resultado. Por lo tanto, debes asignarla a una nueva variable si deseas utilizarla más tarde en tu código.

## Información detallada

El método `substring()` es muy versátil y puede ser utilizado de diferentes maneras, según las necesidades de tu código. Aquí tienes algunas cosas a tener en cuenta al utilizar este método:

- El índice inicial puede ser mayor al índice final, lo que resultará en una cadena con los caracteres en orden inverso. Por ejemplo, si utilizas `cadena.substring(8, 3)`, obtendrás la subcadena "!sot a".

- Si omites el segundo parámetro al utilizar `substring()`, el índice final predeterminado será el último carácter de la cadena original.

- También puedes utilizar valores negativos para los índices, lo que indicará una posición relativa desde el final de la cadena. Por ejemplo, el índice -1 indica el último carácter de la cadena, -2 el penúltimo, y así sucesivamente.

- Si utilizas un índice fuera del rango de la cadena (por ejemplo, 15 en una cadena de 10 caracteres), se producirá un error `IndexOutOfBoundsException`.

## Ver también

- [Documentación oficial de Java sobre el método substring()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Aprender Java: Cadenas](https://www.aprenderaprogramar.com/index.php?option=com_content&view=article&id=662:tutorial-java-cadenas-string-valor-longitud-charat-equals-concat-numericos-iv-cu00515b&catid=68&Itemid=188)
- [Video tutorial: Cómo extraer una subcadena en Java](https://www.youtube.com/watch?v=gHioX6MoRXo)