---
title:                "Arduino: Utilizando expresiones regulares"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has encontrado alguna vez en una situación en la que necesitabas buscar o manipular patrones de texto en un programa de Arduino? ¿O te has visto obligado a revisar manualmente grandes cantidades de datos para encontrar un patrón específico? La respuesta a estos problemas podría ser el uso de expresiones regulares en tus programas de Arduino.

## Cómo hacerlo

Primero, ¿qué son las expresiones regulares? Son patrones de búsqueda que se utilizan para encontrar y manipular texto en cadenas de caracteres. En Arduino, para usar expresiones regulares, necesitarás importar la biblioteca Regex.h. Luego, puedes crear un objeto Regex y utilizar sus métodos para buscar y manipular el texto en tus programas.

Supongamos que tenemos una cadena de datos en formato CSV y queremos extraer solo los valores numéricos. Podemos usar una expresión regular para buscar cualquier número en la cadena y luego convertirlo a un tipo de dato entero o flotante. Aquí está un ejemplo de código:

```
Arduino:
#include <Regex.h>

void setup(){
  Serial.begin(9600);
  String data = "Sensor1,25.35, 45.67, Sensor2,10.20, 53.21";
  Regex regex_obj("[0-9]+\\.[0-9]+");
  while(regex_obj.find(data)){
    float value = atof(regex_obj.peek());
    Serial.println(value);
  }
}

void loop(){
  //Code goes here
}
```

En este ejemplo, primero importamos la biblioteca Regex.h y luego creamos un objeto Regex llamado "regex_obj". Utilizamos la expresión regular "[0-9]+\\.[0-9]+" para buscar cualquier número con un decimal en la cadena de datos. Dentro del bucle, usamos el método "find" para buscar el siguiente valor que coincida con el patrón y luego utilizamos la función "atof" para convertirlo a un tipo de dato flotante. Finalmente, imprimimos el valor en el monitor serie.

El resultado de este ejemplo sería:

```
25.35
45.67
10.20
53.21
```

## Profundizando

Las expresiones regulares pueden ser mucho más complejas y poderosas que el ejemplo anterior. Hay una amplia variedad de caracteres y símbolos que puedes utilizar para crear patrones de búsqueda más específicos. Por ejemplo, puedes buscar un número específico de veces un determinado carácter en una cadena, o buscar patrones de letras y números específicos. Puedes consultar la documentación de la biblioteca Regex.h para obtener más detalles sobre cómo utilizar expresiones regulares en Arduino.

También es importante tener en cuenta que el uso de expresiones regulares puede tener un impacto en el rendimiento de tu programa, especialmente si se utilizan en grandes cantidades de datos. Así que asegúrate de optimizar tus patrones de búsqueda y realizar pruebas para encontrar la mejor solución para tu proyecto.

## Ver también

- Documentación de la biblioteca Regex.h: https://www.arduino.cc/reference/en/libraries/regex/
- Tutorial de expresiones regulares en Arduino (en inglés): https://www.arduino.cc/en/Tutorial/RegexpPatternModifiers