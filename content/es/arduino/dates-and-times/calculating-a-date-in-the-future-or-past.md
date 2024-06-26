---
date: 2024-01-20 17:28:31.254804-07:00
description: "C\xF3mo hacerlo: En el contexto hist\xF3rico, calcular fechas es una\
  \ necesidad humana antigua relacionada con la agricultura, la navegaci\xF3n y la\
  \ astronom\xEDa. En\u2026"
lastmod: '2024-04-05T22:51:13.068996-06:00'
model: gpt-4-1106-preview
summary: "En el contexto hist\xF3rico, calcular fechas es una necesidad humana antigua\
  \ relacionada con la agricultura, la navegaci\xF3n y la astronom\xEDa."
title: "C\xE1lculo de una fecha en el futuro o el pasado"
weight: 26
---

## Cómo hacerlo:
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 3, 1, 2023); // Establece la hora actual a las 10:00:00 del 3 de Enero de 2023
  
  // Calcula una fecha 30 días en el futuro
  time_t future = now() + (30 * SECS_PER_DAY);
  mostrarFecha(future);
  
  // Calcula una fecha 15 días en el pasado
  time_t past = now() - (15 * SECS_PER_DAY);
  mostrarFecha(past);
}

void mostrarFecha(time_t t) {
  Serial.print(day(t));
  Serial.print("/");
  Serial.print(month(t));
  Serial.print("/");
  Serial.print(year(t));
  Serial.println();
}

void loop() {
  // Aquí iría el resto de tu código
}
```

**Resultado:**
```
2/2/2023
19/12/2022
```

## Profundización
En el contexto histórico, calcular fechas es una necesidad humana antigua relacionada con la agricultura, la navegación y la astronomía. En programación, librerías como `TimeLib.h` en Arduino simplifican esta tarea manejando complicaciones como años bisiestos y la conversión entre unidades de tiempo. Alternativas incluyen usar RTC (Real Time Clock) hardware para un manejo más preciso y resiliente del tiempo. La implementación en el código se base en `time_t`, un tipo de dato que representa tiempos en segundos desde la época Unix (1 enero 1970), permitiendo cálculos con operaciones aritméticas simples.

## Ver también
- Time library for Arduino: https://www.pjrc.com/teensy/td_libs_Time.html
- Información sobre `time_t`: https://www.cplusplus.com/reference/ctime/time_t/
- Documentación del Arduino: https://www.arduino.cc/reference/en/
