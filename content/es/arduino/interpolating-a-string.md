---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La interpolación de cadenas permite incorporar variables dentro de una cadena de texto. Los programadores lo hacen para agilizar la concatenación de cadenas y hacer el código más fácil de leer.

## Cómo hacerlo:

El siguiente ejemplo muestra cómo se utiliza la interpolación de cadenas en Arduino.

```Arduino
String nombre = "Juan";
String saludo = "Hola, " + nombre;
Serial.println(saludo);  // El resultado será: Hola, Juan
```

En este caso, hemos usado la operación '+' para concatenar la cadena "Hola, " con el valor de la variable 'nombre'.

## Análisis Profundo:

(1) Contexto Histórico: Antes de la escalada de los lenguajes de programación modernos, la concatenación de cadenas era una tarea manual y tediosa para los programadores. La interpolación de cadenas surgió como una solución para simplificar esta tarea.

(2) Alternativas: En lugar de la interpolación de cadenas, también puede usar la función sprintf(). Sin embargo, el uso de la interpolación puede proporcionar un código más limpio y legible.

```Arduino
char nombre[] = "Juan";
char saludo[20];
sprintf(saludo, "Hola, %s", nombre);
Serial.println(saludo); // El resultado será: Hola, Juan
```

(3) Detalles de Implementación: La configuración de la memoria puede ser crucial cuando se trabaja con la concatenación de cadenas en Arduino. La concatenación de cadenas puede aumentar la utilización de la memoria si se maneja de manera inapropiada.

## Ver También:

Para más detalles sobre la concatenación de cadenas y la interpolación, puede visitar los siguientes enlaces:

3. [Stackoverflow - ¿Cómo concatenar múltiples cadenas de caracteres en Arduino?](https://es.stackoverflow.com/questions/126571/c%C3%B3mo-concatenar-m%C3%BAltiples-cadenas-de-caracteres-en-arduino)