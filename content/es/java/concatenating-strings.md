---
title:                "Java: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una tarea común en la programación que permite combinar varias cadenas de texto en una sola. Esto es especialmente útil para generar mensajes dinámicos o para construir cadenas de consulta en bases de datos. Aprender a concatenar cadenas en Java te ayudará a mejorar tus habilidades de programación y a crear aplicaciones más robustas.

## Cómo hacerlo

En Java, la concatenación de cadenas se realiza mediante el operador de suma (+). Por ejemplo, si queremos combinar las cadenas "Hola" y "mundo", escribiríamos:

```Java
String saludo = "Hola";
String mensaje = saludo + " mundo";

System.out.println(mensaje);
```

La salida de este código sería "Hola mundo". También es posible concatenar más de dos cadenas a la vez:

```Java
String nombre = "Juan";
String apellido = "Pérez";
String mensaje = "Hola " + nombre + " " + apellido;

System.out.println(mensaje);
```

La salida de este código sería "Hola Juan Pérez". Además del operador de suma, también es posible utilizar el método `concat()` de la clase `String` para concatenar cadenas:

```Java
String saludo = "¡Buenos ";
String mensaje = saludo.concat("días!");

System.out.println(mensaje);
```

La salida de este código sería "¡Buenos días!".

## Profundizando

Es importante tener en cuenta que en Java, las cadenas son inmutables, lo que significa que no pueden modificarse una vez creadas. Por lo tanto, cada vez que se realiza una concatenación de cadenas, se crea una nueva cadena en la memoria. Esto puede ser ineficiente si se realizan muchas concatenaciones en un bucle, ya que se estarían creando y descartando constantemente nuevas cadenas.

Para evitar este problema, es recomendable utilizar la clase `StringBuilder`, que permite construir y modificar cadenas de manera eficiente. También es posible utilizar `StringBuffer` en lugar de `StringBuilder`, aunque es menos eficiente ya que es sincronizado.

Otra opción para concatenar cadenas es mediante la función `format()` de la clase `String`. Esta función permite construir cadenas utilizando un formato específico, lo que es especialmente útil para la impresión de fechas y números.

## Ver también

- [Tutorial de Java: Aprende a programar con Java](https://www.aprenderaprogramar.com/curso/java/tutorial-lenguaje-programacion-poo-aprender-programacion-codigo-ejemplos-ejercicios.html)
- [Documentación de Oracle sobre la clase String en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Ejemplos de uso de StringBuilder en Java](https://www.baeldung.com/java-string-builder)
- [Cómo utilizar la función format() en Java](https://www.techiedelight.com/format-strings-in-java/)