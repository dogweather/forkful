---
title:                "Uniendo cadenas de texto"
html_title:           "Java: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si estás aprendiendo Java, es importante entender cómo concatenar cadenas o strings. Concatenar strings te permite combinar varios textos en uno solo, lo que es útil para crear mensajes dinámicos y personalizados en tus programas.

## Cómo hacerlo

Usar el operador "+" es la forma más simple de concatenar strings en Java. También puedes utilizar la función "concat()" para unir dos o más cadenas.

```Java
// Usando el operador "+"
String saludo = "¡Hola ";
String nombre = "Juan!";
String mensaje = saludo + nombre;
System.out.println(mensaje); // Salida: ¡Hola Juan!

// Usando la función "concat()"
String primerNombre = "María";
String segundoNombre = "Fernández";
String nombreCompleto = primerNombre.concat(segundoNombre);
System.out.println(nombreCompleto); // Salida: MaríaFernández
```

Como puedes ver en el ejemplo, al usar el operador "+" se agrega una cadena al final de la otra, mientras que con la función "concat()" se unen en un solo string sin espacios entre ellos.

También puedes utilizar el método "format()" con una sintaxis similar a la de la función "printf()" en C para concatenar strings con otros tipos de datos como números.

```Java
int edad = 27;
String mensaje = String.format("¡Hola, soy Juan y tengo %d años!", edad);
System.out.println(mensaje); // Salida: ¡Hola, soy Juan y tengo 27 años!
```

## Deep Dive

En Java, las cadenas son objetos y no tipos de datos primitivos como en otros lenguajes de programación. Esto significa que las cadenas se almacenan en la memoria de manera diferente y que se pueden utilizar métodos para manipularlas y concatenarlas de maneras más complejas.

El operador "+" tiene una pequeña desventaja en cuanto a rendimiento comparado con la función "concat()", ya que al usar el operador se crean objetos StringBuilder por detrás, lo que puede aumentar el uso de memoria. Por lo tanto, se recomienda utilizar la función "concat()" cuando sea posible.

Otra forma de concatenar strings es usando el método "StringBuilder.append()", que resulta más eficiente que usar el operador "+" ya que se modifica el objeto existente en lugar de crear uno nuevo.

```Java
StringBuilder mensaje = new StringBuilder("¡Hola ");
mensaje.append("Juan!");
System.out.println(mensaje.toString()); // Salida: ¡Hola Juan!
```

Si necesitas concatenar un gran número de strings, el método "StringBuilder.append()" es la mejor opción ya que no se crea un nuevo objeto en cada concatenación, sino que se modifica el mismo objeto cada vez.

## Ver también

- [Documentación oficial de Java sobre cadenas](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Tutorial de W3Schools sobre concatenación de cadenas en Java](https://www.w3schools.com/java/java_strings_concat.asp)
- [Explicación detallada de la diferencia entre el operador "+" y la función "concat()" en Java](https://www.baeldung.com/java-string-add)