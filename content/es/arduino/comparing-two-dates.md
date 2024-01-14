---
title:    "Arduino: Comparando dos fechas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Arduino?

Comparar dos fechas puede ser útil en muchas aplicaciones de Arduino. Por ejemplo, puede que necesites verificar si la fecha actual es anterior o posterior a una fecha específica, o tal vez quieras crear una alarma basada en la hora y fecha. Sea cual sea tu razón, aprender a comparar fechas en Arduino es un conocimiento útil que te permitirá crear proyectos más avanzados.

## ¿Cómo hacerlo?

Para comparar fechas en Arduino, necesitas utilizar el objeto `DateTime` de la librería `DS3231`. Primero, debes crear una instancia de este objeto y asignarle la hora y fecha actual. Luego, puedes utilizar la función `isAfter()` o `isBefore()` para comparar las fechas según tus necesidades.

Aquí tienes un ejemplo de cómo comparar la fecha actual con una fecha específica y mostrar un mensaje en el monitor serial:

```Arduino
#include <DS3231.h>

DS3231 rtc;

void setup() {
  // Iniciar comunicación con RTC
  rtc.begin();
  // Asignar la hora y fecha actual
  rtc.setHour(12);
  rtc.setMinute(0);
  rtc.setSecond(0);
  rtc.setDoW(THURSDAY);
  rtc.setDate(15);
  rtc.setMonth(7);
  rtc.setYear(2021);
}

void loop() {
  // Obtener la fecha actual del RTC
  DateTime now = rtc.now();
  // Comparar con la fecha específica y mostrar mensaje en el monitor serial
  if (now.isAfter(DateTime(2021, 6, 1, 0, 0, 0))) {
    Serial.println("La fecha actual es posterior al 1 de junio de 2021.");
  } else {
    Serial.println("La fecha actual es anterior al 1 de junio de 2021.");
  }
  // Esperar un segundo
  delay(1000);
}
```

El monitor serial mostrará el mensaje "La fecha actual es posterior al 1 de junio de 2021." ya que la fecha actual es posterior a la fecha especificada en el código.

## Profundizando en la comparación de fechas

La librería `DS3231` también incluye funciones para comparar no solo fechas sino también horas, minutos y segundos. Puedes utilizar las funciones `hourIsAfter()`, `minuteIsAfter()` y `secondIsAfter()` para comparar la hora actual con una hora específica.

Además, si estás utilizando una pantalla LCD, puedes mostrar la fecha actual en formato legible utilizando la función `String()`:

```Arduino
// Mostrar fecha actual en pantalla LCD
lcd.print(String(now.day()) + "/" + String(now.month()) + "/" + String(now.year()));
```

Con esta información, puedes crear todo tipo de proyectos que utilicen comparación de fechas en Arduino.

## Ver también

- [Documentación de la librería DS3231 (en inglés)](https://github.com/NorthernWidget/DS3231)
- [Tutorial: Crear una alarma en Arduino utilizando fecha y hora](https://www.prometec.net/ds3231-electronica-proyecto-arduino-alarma/)
- [Proyecto: Reloj despertador con Arduino y pantalla LCD](https://programarfacil.com/blog/arduino-blog/reloj-despertador-arduino/)