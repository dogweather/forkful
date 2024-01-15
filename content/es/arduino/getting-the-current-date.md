---
title:                "Obteniendo la fecha actual"
html_title:           "Arduino: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual?

Si estás trabajando en un proyecto que necesita utilizar la fecha actual, como un reloj o una alarma, es importante saber cómo obtener este dato en Arduino. Afortunadamente, hay funciones incorporadas en el lenguaje de programación de Arduino que permiten obtener la fecha y hora actuales de manera sencilla.

## Cómo hacerlo

Obtener la fecha actual en Arduino requiere el uso de la biblioteca "Time", que se encuentra incluida en el IDE de Arduino. Primero, debemos inicializar la biblioteca en nuestro código:

```Arduino
#include <Time.h>
```

Luego, podemos utilizar la función `now()` para obtener la fecha y hora actuales. Esta función devuelve un objeto `tmElements_t`, el cual contiene los diferentes valores de tiempo, como año, mes, día, hora, entre otros. Por ejemplo, podemos imprimir la fecha actual en el monitor serial utilizando la función `Serial.println()`:

```Arduino
// Inicializamos la biblioteca
#include <Time.h>

void setup() {
  // Inicializamos el monitor serial
  Serial.begin(9600);
  // Obtenemos la fecha actual
  tmElements_t fecha_actual = now();
  // Imprimimos la fecha en formato día/mes/año
  Serial.println("Fecha actual: " + String(fecha_actual.Day) + "/" + String(fecha_actual.Month) + "/" + String(fecha_actual.Year));
}

void loop() {
  // El código en el bucle loop no se ejecutará ya que solo necesitamos obtener la fecha una vez
}
```

La salida en el monitor serial sería: `Fecha actual: 24/09/2021`

## Un poco más profundo

Si queremos personalizar el formato de la fecha y hora que obtenemos, podemos utilizar la función `makeTime()` de la biblioteca "Time". Esta función nos permite crear un objeto `time_t` utilizando los valores de tiempo que deseemos, por ejemplo:

```Arduino
// Inicializamos la biblioteca
#include <Time.h>

void setup() {
  // Inicializamos el monitor serial
  Serial.begin(9600);
  // Creamos un objeto time_t con la fecha y hora específica
  time_t fecha_hora = makeTime(0, 0, 0, 24, 9, 2021);
  // Imprimimos la fecha en formato día/mes/año hora:minutos:segundos
  Serial.println("Fecha y hora específica: " + String(day(fecha_hora)) + "/" + String(month(fecha_hora)) + "/" + String(year(fecha_hora)) + " " + String(hour(fecha_hora)) + ":" + String(minute(fecha_hora)) + ":" + String(second(fecha_hora)));
}

void loop() {
  // El código en el bucle loop no se ejecutará ya que solo necesitamos obtener la fecha una vez
}
```

La salida en el monitor serial sería: `Fecha y hora específica: 24/9/2021 0:0:0`

## Ver también

- [Documentación de la biblioteca Time en el sitio oficial de Arduino](https://www.arduino.cc/reference/es/libraries/time/)
- [Tutorial de GeekyTheory sobre cómo obtener la fecha actual en Arduino](https://www.geekytheory.com/obtener-la-fecha-actual-con-arduino-y-otras-fechas-relevantes/)