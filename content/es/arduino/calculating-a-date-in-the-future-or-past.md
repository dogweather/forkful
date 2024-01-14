---
title:    "Arduino: Calculando una fecha en el futuro o pasado"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o pasado?

Calcular una fecha en el futuro o pasado puede ser útil en proyectos de Arduino que requieran una programación de eventos en una fecha específica. Por ejemplo, puede ser necesario activar una alarma en cierta fecha o programar que un dispositivo se encienda en una fecha determinada. En estos casos, es necesario utilizar el reloj interno de Arduino y hacer cálculos para determinar la fecha deseada.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado en Arduino, es necesario seguir algunos pasos. Primero, se debe asegurar que el reloj interno de Arduino esté configurado correctamente, lo cual se puede hacer utilizando la librería "Time" que viene incluida en el IDE de Arduino. Luego, se deben utilizar las funciones de la librería para obtener la fecha y hora actual. A continuación, se pueden realizar cálculos matemáticos para sumar o restar días, meses o años a la fecha actual y obtener la fecha deseada. Por último, se puede imprimir la fecha calculada en el monitor serial o utilizarla en otras partes del código.

A continuación, se muestra un ejemplo de código para calcular la fecha en 2 días en el futuro:

```
Arduino ...
#include <Time.h>

void setup() {
  Serial.begin(9600);

  // Configurar el reloj interno
  setTime(10, 0, 0, 1, 1, 2019); // HH, MM, SS, DD, MM, YYYY

  // Obtener la fecha y hora actual
  int dia = day();
  int mes = month();
  int año = year();

  // Sumar 2 días a la fecha actual
  dia += 2;

  // Verificar si la fecha calculada es válida
  if (dia <= 31) {
    // Imprimir la fecha
    Serial.print("La fecha en 2 días será: ");
    Serial.print(dia);
    Serial.print("/");
    Serial.print(mes);
    Serial.print("/");
    Serial.println(año);
  }
  else {
    // Si la fecha no es válida, se reiniciará el día a 1 y se sumará 1 al mes
    dia = 1;
    mes += 1;
    // Imprimir la fecha
    Serial.print("La fecha en 2 días será: ");
    Serial.print(dia);
    Serial.print("/");
    Serial.print(mes);
    Serial.print("/");
    Serial.println(año);
  }
}

void loop() {
  // No es necesario utilizar el loop para este ejemplo
}
```

## Profundizando en el cálculo de fechas

Calcular la fecha en el futuro o pasado puede parecer sencillo, pero es importante tener en cuenta algunos detalles. Por ejemplo, es necesario tener en cuenta los meses que tienen 30 o 31 días, así como también el mes de febrero que puede tener 28 o 29 días en caso de ser bisiesto. También es importante considerar el formato de la fecha que se necesita, ya que puede variar dependiendo del país o región.

Otro detalle a tener en cuenta es el uso de la función "setTime()" para configurar el reloj interno de Arduino. Esta función requiere que se le especifiquen los parámetros en el siguiente orden: hora, minuto, segundo, día, mes, año. Por lo tanto, es importante seguir este orden al utilizarla.

## Mira también

- [Librería "Time" de Arduino](https://www.arduino.cc/en/reference/time)
- [Cómo utilizar el reloj interno de Arduino](https://www.arduino.cc/en/Tutorial/TimeRTC)
- [Ejemplos de cálculos de fechas en Arduino](https://gist.github.com/firesofmay/f7b46588bdba165ec65e85e0b9053196)