---
title:    "Java: Uniendo cadenas de caracteres"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Java?

Concatenar cadenas es una habilidad esencial en programación Java que permite combinar diferentes cadenas de texto en una sola. Esto es especialmente útil cuando se trabaja con texto dinámico y se necesita formatear la salida de datos.

## Cómo hacerlo

Para concatenar cadenas en Java, se utiliza el operador "+". Este operador se utiliza para unir dos o más cadenas de texto. Veamos un ejemplo:

```Java
String primeraCadena = "¡Hola ";
String segundaCadena = "mundo!";
String saludo = primeraCadena + segundaCadena;
System.out.println(saludo);
```
El resultado en la consola sería: ¡Hola mundo!

En este ejemplo, se crearon dos cadenas "Hola" y "mundo" y se unieron utilizando el operador "+". Luego, se asignó el resultado a una nueva variable llamada "saludo" y se imprime en la consola.

Otra forma de concatenar cadenas es utilizando el método "concat()". Este método toma una cadena como parámetro y la une a la cadena existente. Ejemplo:

```Java
String primeraCadena = "¡Hola ";
String segundaCadena = "mundo!";
String saludo = primeraCadena.concat(segundaCadena);
System.out.println(saludo);
```
El resultado en la consola sería el mismo que el ejemplo anterior.

## Inmersión profunda

Mientras que el operador "+" y el método "concat()" son las formas más comunes de concatenar cadenas en Java, también hay otros métodos disponibles, como "StringBuilder" y "StringBuffer". Estos métodos son más eficientes cuando se trabaja con grandes cantidades de cadenas.

También es importante tener en cuenta que en Java, las cadenas son inmutables, lo que significa que no se pueden cambiar una vez que se han creado. Por lo tanto, cada vez que se concatena una cadena, se está creando una nueva cadena en la memoria.

## Ver también
- Tutorial de concatenación de cadenas en Java: https://www.adictosaltrabajo.com/2012/03/14/strings-en-java/
- Documentación oficial de Java sobre cadenas: https://docs.oracle.com/javase/tutorial/java/data/strings.html