---
title:                "Encontrar la longitud de una cadena"
html_title:           "Java: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En programación, encontrar la longitud de una cadena de texto se refiere a determinar la cantidad de caracteres que contiene dicha cadena. Los programadores suelen hacer esto para realizar diferentes tareas, como verificar si una cadena supera un límite de caracteres establecido o para manipular su contenido de manera más precisa.

## ¿Cómo hacerlo?

Para encontrar la longitud de una cadena en Java, se puede utilizar el método ```length()``` que viene incluido en la clase ```String```. A continuación, se muestra un ejemplo de cómo utilizar este método:

```Java
String cadena = "Hola mundo";
int longitud = cadena.length();
System.out.println("La longitud de la cadena es: " + longitud);
```
La salida de este código sería "La longitud de la cadena es: 10", ya que la palabra "Hola mundo" tiene 10 caracteres.

## Profundizando

En el pasado, encontrar la longitud de una cadena podía ser una tarea más tediosa, ya que se tenía que recorrer toda la cadena contando cada uno de sus caracteres. Con el avance de la tecnología y la optimización de los lenguajes de programación, se han creado métodos más eficientes como el mencionado anteriormente.

Un método alternativo para obtener la longitud de una cadena en Java es utilizando la función ```public int length()``` de la clase ```StringBuilder```, que también devuelve la cantidad de caracteres de una cadena. Sin embargo, es importante tener en cuenta que esta función no es tan eficiente como la de la clase ```String```, principalmente porque puede producir más código innecesario.

En cuanto a la implementación de la función ```length()```, es importante destacar que utiliza contadores internos en lugar de tener que recorrer manualmente la cadena de texto, lo que lo hace más rápido y eficiente.

## Ver también

Si quieres saber más sobre cómo trabajar con cadenas de texto en Java, puedes revisar la documentación oficial de Oracle: https://docs.oracle.com/javase/tutorial/java/data/strings.html