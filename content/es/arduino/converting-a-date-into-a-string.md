---
title:                "Arduino: Convirtiendo una fecha en una cadena"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por qué

Convertir una fecha en una cadena de caracteres puede ser útil en muchas aplicaciones de Arduino. Por ejemplo, podrías querer mostrar la fecha actual en una pantalla LCD o utilizarla como parte de una cadena de texto en un proyecto.

# Cómo hacerlo

Para convertir una fecha en una cadena de caracteres, primero necesitas usar la librería "Time.h". Esta librería incluye una función llamada "timeString()", que convierte la fecha en una cadena en formato de 24 horas.

```Arduino
#include <Time.h>

void setup() {
  // Inicializa la comunicación con el reloj en tiempo real
  setTime(15, 30, 0, 1, 1, 2020);
}

void loop() {
  // Lee la fecha actual y conviértela en una cadena de caracteres
  String fecha = timeString();

  // Imprime la cadena en el monitor serial
  Serial.println("La fecha actual es: " + fecha);
  delay(1000);
}
```

La salida de este código sería: "La fecha actual es: 15:30:00 1/1/2020". Puedes cambiar el formato de la fecha utilizando diferentes funciones de la librería, como "day()", "month()", "year()", etc.

# Profundizando

La librería "Time.h" también te permite convertir una fecha en un objeto de tipo "time_t". Este objeto puede ser utilizado para realizar operaciones matemáticas con fechas, como sumar o restar días.

Por ejemplo, si quisieras mostrar la fecha actual y la fecha de mañana en una pantalla LCD, podrías hacerlo de la siguiente manera:

```Arduino
#include <Time.h>

void setup() {
  // Inicializa la comunicación con el reloj en tiempo real
  setTime(15, 30, 0, 1, 1, 2020);
}

void loop() {
  // Lee la fecha actual y conviértela en un objeto time_t
  time_t fechaActual = now();

  // Suma un día al objeto y conviértelo en una cadena de caracteres
  String fechaManana = timeString(fechaActual + SECS_PER_DAY);

  // Imprime ambas fechas en la pantalla LCD
  lcd.print("Fecha actual: ");
  lcd.println(fechaActual);
  lcd.print("Fecha de mañana: ");
  lcd.println(fechaManana);

  delay(1000);
}
```

# Ver también

- Documentación oficial de la librería "Time.h": [https://www.arduino.cc/reference/en/libraries/time/](https://www.arduino.cc/reference/en/libraries/time/)
- Tutorial sobre cómo usar la librería "Time.h" en proyectos de Arduino: [https://create.arduino.cc/projecthub/g35ta/display-date-and-time-from-a-real-time-clock-rtc-0c0fd4](https://create.arduino.cc/projecthub/g35ta/display-date-and-time-from-a-real-time-clock-rtc-0c0fd4)

¡Esperamos que este artículo te haya sido útil para aprender cómo convertir una fecha en una cadena de caracteres en Arduino! Con esta información, podrás añadir nuevas funcionalidades a tus proyectos y mostrar la fecha actual de forma precisa y detallada. ¡A seguir aprendiendo y creando con Arduino!