---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Encontrar el tamaño de una cadena de texto ("string") significa determinar cuántos caracteres contiene. Los programadores lo hacen para manipular o validar datos. Por ejemplo, puedes comprobar la longitud para asegurarte de que un campo de entrada del usuario no esté vacío o que no supere un límite específico.

## ¿Cómo hacerlo?
En Java, puedes encontrar la longitud de una cadena usando el método `length()`. Aquí tienes un ejemplo:

```Java
String miCadena = "¡Hola, mundo!";
int tamaño = miCadena.length();
System.out.println("La longitud de la cadena es: " + tamaño);
```
El resultado en la consola será:

```
La longitud de la cadena es: 13
```

## Análisis profundo
1. *Contexto histórico*: Desde los primeros días de Java, el método `length()` ha sido la forma preferida para encontrar la longitud de una cadena. 
2. *Alternativas*: Pese a ello, puedes usar `toCharArray()` para convertir la cadena a un array de caracteres y luego usar su propiedad `length`.
```Java
int tamañoAlternativo = miCadena.toCharArray().length;
```
Sin embargo, `length()` es más eficiente y directo. 
3. *Detalles de implementación*: Internamente, `length()` devuelve el valor del campo `count` del objeto `String`, que se establece al crear la cadena.

## Ver también
1. [Documentación oficial de Java para String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
2. [Tutorial de Oracle sobre cadenas de texto](https://docs.oracle.com/javase/tutorial/java/data/strings.html)

Recuerda, como siempre, que la mejor manera de aprender es practicando. ¡Manos a la obra, campeón!