---
title:                "Capitalizando una cadena"
html_title:           "Java: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena en java simplemente significa convertir la primera letra de cada palabra en mayúscula. Los programadores a menudo capitalizan cadenas para hacer que los resultados sean visualmente más atractivos y legibles.

## ¿Cómo hacerlo?

Para capitalizar una cadena en Java, podemos usar el método `toUpperCase()` junto con el método `substring()` para obtener la primera letra de cada palabra y convertirla a mayúscula. Por ejemplo:

```Java
String str = "este es un ejemplo";
String[] words = str.split(" ");
String captitalizedString = "";
for (String word : words) {
    captitalizedString += Character.toUpperCase(word.charAt(0)) + word.substring(1)+ " ";
}
System.out.println(capitalizedString);
```
La salida sería:
```
Este Es Un Ejemplo 
```

## Más información

La capitalización de cadenas ha sido una práctica común en programación desde los días de las máquinas de escribir. Además del método mencionado anteriormente, también podemos capitalizar una cadena usando la clase `StringBuilder` o la función `toUpperCase()` de la clase `String`. Si bien la capitalización de cadenas es útil para mejorar la legibilidad, también debemos tener cuidado al manejar cadenas en diferentes idiomas que tienen reglas de capitalización específicas.

## Vea también 

- [Java String Class](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [StringBuilder Class](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/StringBuilder.html)
- [Java Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)