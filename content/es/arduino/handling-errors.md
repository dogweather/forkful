---
title:                "Manejo de errores"
date:                  2024-01-26T00:23:44.186505-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El manejo de errores en tus programas detecta los problemas imprevistos que intentarán hacerte tropezar. Lo haces para evitar que tu Arduino tenga un colapso cuando ocurra lo inesperado.

## Cómo hacerlo:

Digamos que tu Arduino está leyendo un sensor que ocasionalmente puede producir valores fuera del rango. Así es cómo podrías manejarlo:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // El valor está dentro del rango, proceder con el procesamiento
  Serial.println(sensorValue);
} else {
  // El valor está fuera del rango, manejar el error
  Serial.println("Error: Valor del sensor fuera de rango.");
}
```
Salida de muestra:
```
523
Error: Valor del sensor fuera de rango.
761
```

## Análisis en Profundidad

El manejo de errores no siempre ha sido tan sencillo. En los primeros días, los desarrolladores a menudo ignoraban los errores, lo que llevaba al temido "comportamiento indefinido". A medida que la programación evolucionó, también lo hicieron las herramientas — ahora tienes excepciones en muchos idiomas, pero todavía es un 'chequéalo-primero' a la vieja escuela en el mundo Arduino debido a las restricciones de hardware y raíces en C++.

En la programación de Arduino, a menudo ves declaraciones `if-else` para el manejo de errores. Pero hay alternativas: usar la función `assert` para detener la ejecución si una condición falla o diseñar salvaguardas dentro de la configuración de tu hardware en sí.

Al implementar el manejo de errores, considera el impacto de detener el programa frente a permitir que continúe con un estado predeterminado o seguro. Hay una compensación, y la elección correcta depende del daño potencial de las interrupciones versus la operación incorrecta.

## Ver También

Profundiza en la detección y manejo de errores con estos:

- Referencia del Lenguaje Arduino: https://www.arduino.cc/reference/en/
- Una mirada más profunda al manejo de errores de Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Manejo de Errores en C++: https://en.cppreference.com/w/cpp/error/exception

Esto debería darte los conocimientos y la confianza para evitar las trampas de los errores en tus aventuras con Arduino.
