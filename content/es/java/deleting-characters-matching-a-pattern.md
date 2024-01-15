---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Java: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías eliminar caracteres que coincidan con un patrón en Java?

Eliminar caracteres que coincidan con un patrón es una tarea común en el desarrollo de software. Puede ser útil para limpiar y validar entradas de usuario o para formatear cadenas de texto de manera precisa. Al seguir los pasos adecuados, puedes lograr esto de manera sencilla en Java.

## Cómo hacerlo en Java

```java
// Importar la clase Scanner de Java
import java.util.Scanner;

// Crear un objeto Scanner para leer la entrada del usuario
Scanner input = new Scanner(System.in);

// Pedir al usuario que ingrese una cadena de texto
System.out.println("Ingresa una cadena de texto:");
String texto = input.nextLine(); // Guardar la entrada del usuario en una variable

// Pedir al usuario que ingrese un patrón para eliminar
System.out.println("Ingresa un patrón a eliminar:");
String patron = input.nextLine(); // Guardar la entrada del usuario en una variable

// Reemplazar todas las coincidencias del patrón con una cadena vacía
String resultado = texto.replaceAll(patron, "");

// Imprimir el resultado final
System.out.println("El texto resultante es: " + resultado);
```

**Entrada de usuario:**

```
Ingresa una cadena de texto:
Hola mundo
Ingresa un patrón a eliminar:
l
```

**Salida:**

```
El texto resultante es: Ho mundo
```

## Profundizando en la eliminación de caracteres que coinciden con un patrón

Java ofrece varios métodos para eliminar caracteres que coincidan con un patrón en una cadena de texto. Uno de ellos es el método `replaceAll()` que utilizamos en el ejemplo anterior. Este método reemplaza todas las coincidencias de un patrón con una cadena especificada.

Otro método útil es `replace()`, que solo reemplaza la primera coincidencia del patrón en la cadena. También hay métodos específicos para eliminar espacios en blanco `trim()` y eliminar caracteres especiales `replaceChars()`. Puedes experimentar con diferentes métodos y ver cuál se ajusta mejor a tus necesidades.

## Ver también

- [Documentación de Java para el método `replaceAll()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Documentación de Java para el método `replace()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-java.lang.CharSequence-java.lang.CharSequence-)
- [Tutorial de Java: Cadena de texto y sus métodos](https://www.youtube.com/watch?v=oHL4C9mwNjw)