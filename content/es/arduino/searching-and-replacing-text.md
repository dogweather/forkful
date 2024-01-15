---
title:                "Buscando y reemplazando texto"
html_title:           "Arduino: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en la situación de tener que hacer cambios repetitivos en tu código de Arduino? ¿Te gustaría ahorrar tiempo y esfuerzo en esa tarea? ¡Entonces sigue leyendo porque hoy te voy a mostrar cómo utilizar la función de búsqueda y reemplazo de texto en Arduino!

## Cómo hacerlo

Para buscar y reemplazar texto en tu código de Arduino, sigue estos sencillos pasos:

1. Abre tu programa de Arduino.
2. En la barra de menú, ve a Editar y selecciona "Buscar y reemplazar" o simplemente utiliza el atajo de teclado "Ctrl + H".
3. En la ventana que se abre, ingresa el texto que quieres buscar en el campo "Buscar" y el texto de reemplazo en el campo "Reemplazar".
4. Selecciona la opción "Todo el proyecto" en la sección "Ámbito" si quieres buscar y reemplazar en todo el código.
5. Haz clic en el botón "Reemplazar todo" para reemplazar todas las instancias del texto buscado en tu código.

¡Y eso es todo! Ahora puedes hacer múltiples cambios en tu código en cuestión de segundos.

```Arduino
// Ejemplo de búsqueda y reemplazo en un código de Arduino

#include <Servo.h>

Servo motor; // Definir el servo motor

void setup() {
  motor.attach(9); // Asignar el pin 9 al motor
}

void loop() {
  motor.write(90); // Mover el motor a 90 grados
  delay(1000);
  motor.write(0); // Mover el motor a 0 grados
  delay(1000);
  motor.write(180); // Mover el motor a 180 grados
  delay(1000);
}
```

En este ejemplo, usamos la función de búsqueda y reemplazo para cambiar el pin del motor de 9 a 10. Simplemente buscamos "9" y lo reemplazamos por "10", evitando así tener que editar cada línea manualmente.

## Profundizando

La función de búsqueda y reemplazo de texto en Arduino también te permite utilizar expresiones regulares. Esto puede serte útil si quieres buscar y reemplazar patrones específicos en tu código.

Por ejemplo, puedes reemplazar todas las variables de tipo "int" por "float" escribiendo en el campo de búsqueda la expresión regular ```\b(int)\b``` y en el campo de reemplazo la palabra "float". Esto reemplazará todas las instancias de "int" que estén rodeadas por límites de palabra por "float".

Otra opción útil es la de utilizar la función de búsqueda y reemplazo para renombrar variables en tu código. Por ejemplo, si quieres cambiar el nombre de una variable de "velocidad" a "vel", simplemente escribe "velocidad" en el campo de búsqueda y "vel" en el de reemplazo.

¡Experimenta con las expresiones regulares y descubre cómo pueden agilizar tus tareas de edición de código en Arduino!

## Ver también

- Documentación oficial de búsqueda y reemplazo en Arduino: https://www.arduino.cc/reference/en/language/functions/communication/betweenupper/
- Tutorial de expresiones regulares en Arduino: https://www.dummies.com/programming/arduino/how-to-use-regular-expressions-in-arduino/