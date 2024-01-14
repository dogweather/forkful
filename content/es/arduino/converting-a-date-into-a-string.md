---
title:    "Arduino: Convirtiendo una fecha en una cadena"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Existen varias razones por las que es importante aprender a convertir una fecha en una cadena de texto en Arduino. Por ejemplo, puede ser útil para mostrar la fecha actual en un reloj digital o para guardar la fecha en una tarjeta SD para su posterior análisis.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Arduino, podemos utilizar la función `sprintf()`. Esta función toma una serie de valores y los convierte en una cadena de texto según un formato especificado. Aquí hay un ejemplo de cómo convertir la fecha actual en una cadena de texto en formato DD/MM/AA:

```Arduino
#include <Time.h>
#include <TimeLib.h>

void setup() {
  // Inicializar el reloj en tiempo real
  setTime(12, 0, 0, 1, 1, 2020);
  // Obtener la fecha actual en formato Unix
  time_t t = now();
  // Crear una variable para guardar la fecha en cadena de texto
  char fecha[9];
  // Utilizar sprintf() para convertir la fecha en una cadena de texto
  sprintf(fecha, "%02d/%02d/%02d", day(t), month(t), year(t));
  // Imprimir la cadena de texto en el monitor serial
  Serial.println(fecha);
}

void loop() {
  // Nada que hacer en el bucle principal
}
```

Después de cargar este código en tu Arduino y abrir el monitor serial, deberías ver la fecha actual en formato DD/MM/AA cada vez que reinicies el Arduino.

## Profundizando

La función `sprintf()` toma una cadena de formato y una lista de valores. Los valores pueden ser variables, constantes o incluso expresiones matemáticas. La cadena de formato especifica cómo los valores deben ser convertidos en una cadena de texto. Por ejemplo, `%02d` indica que el valor debe tener dos dígitos y, de no ser así, se rellenará con ceros.

Puedes encontrar más información sobre los formatos disponibles para `sprintf()` en la [documentación](https://www.cplusplus.com/reference/cstdio/sprintf/) de C++.

## Ver también

- [Cómo usar un reloj en tiempo real con Arduino](https://www.instructables.com/id/Real-Time-Clock-DS1307-With-Arduino-Using-LCD/)
- [Cómo guardar datos en una tarjeta SD con Arduino](https://www.arduino.cc/en/tutorial/SimpleDataLogging)
- [Documentación sobre la función sprintf()](https://www.cplusplus.com/reference/cstdio/sprintf/)