---
title:                "Concatenando cadenas"
html_title:           "Arduino: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En programación, a menudo es necesario unir diferentes cadenas de texto en una sola, ya sea para imprimir mensajes más complejos o para crear estructuras de datos. La concatenación de cadenas es una habilidad clave para cualquier programador y es especialmente útil al trabajar con Arduino, ya que nos permite crear mensajes personalizados para nuestros proyectos.

## Cómo hacerlo

Para concatenar cadenas de texto en Arduino, utilizamos el operador `+`, similar a como se hace en otros lenguajes de programación. Veamos un ejemplo de cómo podemos imprimir un mensaje personalizado utilizando la función `Serial.println()`:

```Arduino
String nombre = "Juan";
String saludo = "¡Hola " + nombre + "!";
Serial.println(saludo);
```

En este ejemplo, hemos creado dos cadenas de texto, `nombre` y `saludo`, y las hemos concatenado utilizando el operador `+`. Al imprimir la variable `saludo` utilizando la función `Serial.println()`, obtendremos el siguiente resultado:

```
¡Hola Juan!
```

Podemos usar este mismo método para crear mensajes más complejos, como por ejemplo, mostrar la temperatura actual en un proyecto de Arduino:

```Arduino
float temperatura = 25.5;
String mensaje = "La temperatura actual es: " + String(temperatura) + " grados Celsius";
Serial.println(mensaje);
```

En este caso, hemos convertido la variable `temperatura` a una cadena de texto utilizando la función `String()`, y luego la hemos concatenado con el resto del mensaje.

## Profundizando

Al concatenar cadenas en Arduino, es importante tener en cuenta el uso de memoria. Cada vez que concatenamos dos cadenas, se crea una nueva cadena en la memoria, por lo que es importante hacer uso eficiente de esta. Si se necesitan concatenar varias cadenas, es mejor utilizar la función `concat()` en lugar del operador `+`, ya que esta última puede tener un impacto negativo en el rendimiento del proyecto.

Otro aspecto importante es el tipo de dato de las variables que estamos concatenando. En el ejemplo anterior, utilizamos la función `String()` para convertir la variable `temperatura` a una cadena de texto, pero esto solo es necesario si la variable no es de tipo `String`. Si ya trabajamos con variables de tipo `String`, el proceso de concatenación se simplifica.

## Vea también

- [Tutorial de concatenación de cadenas en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Manipulación de cadenas en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Operadores de concatenación en otros lenguajes de programación](https://www.w3schools.com/jsref/jsref_concat_string.asp)