---
title:    "Java: Uniendo cadenas"
keywords: ["Java"]
---

{{< edit_this_page >}}

## ¿Por qué? 

La concatenación de cadenas es una técnica muy útil en la programación, que nos permite unir varias cadenas de texto en una sola. Esto puede ser útil en situaciones como la creación de mensajes personalizados, la generación de informes o la construcción de URLs. 

## Cómo hacerlo:

Para concatenar cadenas en Java, podemos usar el operador de suma (+) o el método "concat". Veamos un ejemplo utilizando el operador de suma: 

```Java 
String nombre = "María";
String apellido = "García";
String nombreCompleto = nombre + " " + apellido;
System.out.println(nombreCompleto); 
```
Output: María García

En este caso, hemos utilizado el operador de suma para unir las variables "nombre" y "apellido", separándolas con un espacio en blanco. El resultado es una nueva cadena llamada "nombreCompleto", que contiene el nombre completo de la persona.

También podemos utilizar el método "concat" de la siguiente manera: 

```Java 
String mensaje = "¡Hola, ";
String nombre = "Juan";
String saludo = mensaje.concat(nombre).concat("!");
System.out.println(saludo);
```
Output: ¡Hola, Juan!

Con el método "concat", no es necesario añadir manualmente los espacios entre las cadenas, ya que el método se encarga de ello. 

## Profundizando en la concatenación de cadenas: 

Además de los operadores y métodos mencionados anteriormente, también podemos utilizar la clase "StringBuilder" en Java para concatenar cadenas de manera eficiente. Esto es especialmente útil cuando necesitamos construir una cadena en base a múltiples variables o cuando necesitamos añadir un gran número de cadenas a una misma variable. 

Un tip útil para mejorar el rendimiento de la concatenación de cadenas es evitar el uso de operadores o métodos dentro de bucles, ya que esto puede resultar en un aumento en el uso de memoria y tiempo de ejecución. En su lugar, es recomendable utilizar la clase "StringBuilder" para construir la cadena externamente al bucle y simplemente añadir la variable al final. 

## Ver también: 

- [Documentación oficial de Java sobre la concatenación de cadenas](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Más ejemplos de concatenación de cadenas en Java](https://www.geeksforgeeks.org/stringconcat-method-in-java-with-examples/)
- [Métodos útiles de la clase "StringBuilder"](https://www.tutorialspoint.com/java/lang/stringbuilder_append.htm)