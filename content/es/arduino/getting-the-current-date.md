---
title:    "Arduino: Obteniendo la fecha actual."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si estás trabajando en un proyecto de Arduino que requiere saber la fecha actual, es importante saber cómo obtenerla correctamente. La fecha actual puede ser útil en una variedad de proyectos, como un reloj digital o un programa de control de riego.

## Cómo hacerlo

Para obtener la fecha actual en Arduino, se utiliza la función `millis()`, que devuelve la cantidad de milisegundos desde que se encendió la placa. A continuación, se debe hacer algunos cálculos para convertir los milisegundos en una fecha legible.

Aquí hay un ejemplo de código para obtener la fecha actual y mostrarla en el monitor serie del Arduino:

```
Arduino
unsigned long millis_dia = 86400000; // cantidad de milisegundos en un día
unsigned long millis_inicio = millis(); // obtiene la cantidad de milisegundos desde el encendido
unsigned long dias = millis_inicio / millis_dia; // convierte los milisegundos en días
Serial.print("La cantidad de días desde el encendido es: ");
Serial.println(dias); // imprime el resultado en el monitor serie
```

La salida en el monitor serie se verá así:

```
La cantidad de días desde el encendido es: 2
```

## Profundizando

Además de utilizar la función `millis()`, también se puede utilizar la librería `Time` de Arduino que permite obtener la fecha actual con mayor precisión y en un formato más legible. Esta librería incluye funciones como `hour()`, `minute()`, `day()`, `month()` y `year()` que devuelven la hora, minutos, día, mes y año respectivamente.

Aquí hay un ejemplo de código utilizando la librería `Time` para obtener la fecha y hora actual y mostrarla en el monitor serie:

```
Arduino
#include <Time.h> // incluir la librería

void setup() {
  Serial.begin(9600); // iniciar el monitor serie
  setTime(12, 0, 0, 1, 1, 2020); // establecer la fecha y hora actual
}

void loop() {
  // obtener la fecha y hora actual
  int segundos = second();
  int minutos = minute();
  int horas = hour();
  int dia = day();
  int mes = month();
  int anio = year();
  
  // mostrar la fecha y hora en el monitor serie
  Serial.print("La fecha actual es: ");
  Serial.print(mes);
  Serial.print("/");
  Serial.print(dia);
  Serial.print("/");
  Serial.println(anio);
  Serial.print("La hora actual es: ");
  Serial.print(horas);
  Serial.print(":");
  Serial.print(minutos);
  Serial.print(":");
  Serial.println(segundos);
  
  delay(1000); // esperar 1 segundo antes de volver a obtener la fecha y hora
}
```

La salida en el monitor serie se verá así:

```
La fecha actual es: 1/1/2020
La hora actual es: 12:0:4
```

## Ver también
- [Página de documentación de Arduino sobre la función `millis()`](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Página de documentación de Arduino sobre la librería `Time`](https://www.arduino.cc/reference/en/libraries/time/)