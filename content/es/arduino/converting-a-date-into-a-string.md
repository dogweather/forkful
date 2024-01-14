---
title:                "Arduino: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si estás incursionando en la programación de Arduino, es posible que te hayas preguntado cómo convertir una fecha en una cadena de texto. Esto puede ser útil si quieres mostrar la fecha en una pantalla LCD o almacenarla en una variable para utilizarla más adelante.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Arduino, necesitarás usar la función `sprintf`. Esta función permite dar formato a una cadena de texto según un patrón establecido. En este caso, utilizaremos el patrón `"%02d/%02d/%02d"` para mostrar la fecha en formato `dd/mm/aa`.

```
#include <Time.h>
#include <TimeLib.h>

void setup() {
  // Inicializar el reloj
  setTime(20, 15, 30, 12, 07, 2018); // 20:15:30, 12 de julio de 2018
  // Convertir fecha a cadena de texto
  char fechaString[10];
  sprintf(fechaString, "%d/%02d/%02d", day(), month(), year());
  // Imprimir cadena de texto
  Serial.println(fechaString); // Salida: 12/07/18
}

void loop() {}
```

## Profundizando

La función `sprintf` utiliza los siguientes identificadores para dar formato a la cadena de texto:

- `%d` para números enteros.
- `%02d` para números enteros con un cero antes si es necesario (útil para días y meses).
- `%02d` para números enteros con un cero antes si es necesario (útil para el año en formato de 2 dígitos).
- `%s` para cadenas de texto.
- `%02s` para cadenas de texto con un cero antes si es necesario (útil para meses en formato de 2 dígitos).

En este caso, utilizamos la función `day()`, `month()` y `year()` de la librería `Time` para obtener la fecha actual del sistema en formato numérico.

¡Ahora ya sabes cómo convertir una fecha en una cadena de texto en Arduino!

## Ver también

- [Formato de cadenas de texto en C](https://www.dummies.com/programming/c/how-to-format-text-in-c-programming/)
- [Función sprintf en Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)