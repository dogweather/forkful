---
title:                "Java: Capitalizando una cadena"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#¿Por qué capitalizar una cadena de texto en Java?

Capitalizar una cadena de texto se refiere a convertir la primera letra de cada palabra a mayúscula y dejar el resto en minúscula. Aunque puede parecer una tarea simple, es importante conocer cómo hacerlo en Java para poder manipular eficientemente las cadenas de texto en un programa.

#Cómo hacerlo en Java

Para capitalizar una cadena de texto en Java, se puede utilizar el método `toUpperCase()` de la clase `Character` para convertir la primera letra en mayúscula. Luego, se puede utilizar el método `toLowerCase()` para convertir el resto de la palabra en minúscula. A continuación se muestra un ejemplo de cómo capitalizar una cadena de texto en Java:

```
String cadena = "hola, mundo!";
String[] palabras = cadena.split(" ");
String nuevaCadena = "";
for (String palabra : palabras) {
    char primeraLetra = palabra.charAt(0);
    char primeraLetraMayus = Character.toUpperCase(primeraLetra);
    String restoPalabra = palabra.substring(1);
    restoPalabra = restoPalabra.toLowerCase();
    nuevaCadena += primeraLetraMayus + restoPalabra + " ";
}
System.out.println(nuevaCadena.trim());
```

Este código tomará la cadena de texto "hola, mundo!" y la convertirá en "Hola, Mundo!". Como se puede ver, se utilizó el método `split()` para separar la cadena por palabras y luego un ciclo for para iterar sobre cada palabra y realizar la conversión.

#Profundizando en la capitalización de cadenas de texto

Además de capitalizar la primera letra de cada palabra, también se puede utilizar el método `toUpperCase()` para convertir toda la cadena de texto a mayúsculas. Esto puede ser útil para fines de comparación o para dar un formato específico a una cadena.

Otra forma de capitalizar una cadena de texto es utilizando la clase `StringBuffer`. Esta clase tiene un método llamado `replace()` que permite reemplazar caracteres específicos en una cadena. Utilizando este método, se pueden reemplazar las primeras letras de cada palabra por su versión en mayúscula.

En resumen, capitalizar una cadena de texto en Java puede ser útil para presentar información de manera más coherente o para realizar comparaciones más precisas en un programa.

#Ver también

- [Documentación oficial de Java para la clase Character](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [Método replace de la clase StringBuffer](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html#replace-int-int-java.lang.String-)
- [Tutorial sobre manipulación de cadenas en Java](https://www.javatpoint.com/java-string-tutorial)