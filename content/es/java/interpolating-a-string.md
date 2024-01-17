---
title:                "Interpolación de una cadena"
html_title:           "Java: Interpolación de una cadena"
simple_title:         "Interpolación de una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

# ¿Qué es y por qué lo hacemos? 
Interpolar una cadena en Java es cuando insertamos valores variables dentro de una cadena de texto. Los programadores hacen esto para crear cadenas dinámicas que pueden cambiar con los valores de las variables.

# Cómo hacerlo: 
Podemos interpolar una cadena en Java utilizando el operador "+" para concatenar la cadena con las variables. También podemos utilizar la función "String.format()" para especificar el formato de salida de la cadena interpolada. Aquí hay un ejemplo de código y su salida:

```Java
// Usando el operador de concatenación "+"
String nombre = "María";
int edad = 28;
System.out.println("Hola, mi nombre es " + nombre + " y tengo " + edad + " años.");
```
Salida: Hola, mi nombre es María y tengo 28 años.

```Java
// Usando la función "String.format()"
String nombre = "Juan";
double altura = 1.78;
System.out.println(String.format("Hola, soy %s y mido %.2f metros.", nombre, altura));
```
Salida: Hola, soy Juan y mido 1.78 metros.

# Profundizando: 
Interpolar cadenas en Java no siempre fue una opción. Antes de la versión 8 de Java, se utilizaba el método "concat()" de la clase String para concatenar cadenas. Otra alternativa es el método "StringBuilder.append()" que permite construir cadenas dinámicamente. En cuanto a la implementación, Java utiliza el método "String.format()" para formatear la cadena interpolada según los valores de las variables.

# Ver también: 
- [Oracle - Documentación de la clase String en Java] (https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [W3Schools - Tutorial sobre cómo interpolar cadenas en Java] (https://www.w3schools.com/java/java_strings_concatenation.asp)
- [Java Code Geeks - Artículo sobre la interpolación de cadenas en Java 8] (https://www.javacodegeeks.com/2015/02/string-interpolation-in-java.html)