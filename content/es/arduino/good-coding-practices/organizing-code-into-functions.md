---
date: 2024-01-26 01:08:36.963109-07:00
description: "Organizar el c\xF3digo en funciones significa dividir tu c\xF3digo en\
  \ bloques reutilizables, cada uno realizando una tarea espec\xEDfica. Los programadores\
  \ hacen\u2026"
lastmod: '2024-03-13T22:44:59.338734-06:00'
model: gpt-4-1106-preview
summary: "Organizar el c\xF3digo en funciones significa dividir tu c\xF3digo en bloques\
  \ reutilizables, cada uno realizando una tarea espec\xEDfica."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
Imagina que quieres hacer parpadear un LED. Sin funciones, tu `loop` es una mezcla desordenada. Con funciones, está ordenado. Así es cómo:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, SALIDA);
}

void loop() {
  parpadearLED(500); // Hace parpadear el LED cada 500ms
}

// Función para hacer parpadear un LED
void parpadearLED(int tiempoDeRetraso) {
  digitalWrite(LED_PIN, ALTO);
  delay(tiempoDeRetraso);
  digitalWrite(LED_PIN, BAJO);
  delay(tiempoDeRetraso);
}
```

Salida de muestra: Tu LED está parpadeando alegremente, y el propósito del código es claro a primera vista.

## Profundizando
Antes de las funciones, la programación era un viaje por carretera lineal; veías cada bache de principio a fin. Después de las funciones, es más como saltar en vuelos - vas directo a las partes importantes. Históricamente, las subrutinas (funciones tempranas) fueron una revolución en la programación, permitiendo a los codificadores evitar repetirse – ese es el principio DRY, No Te Repitas (Don’t Repeat Yourself). Las alternativas a las funciones podrían incluir macros o el uso de clases para la programación orientada a objetos (OOP). ¿El detalle técnico? Cuando defines una función, le estás dando al compilador un plano para ejecutar una tarea. Con Arduino, a menudo estás definiendo funciones void que actúan como comandos simples para un microcontrolador, pero las funciones también pueden devolver valores, haciéndolas más versátiles.

## Ver También
Para más información sobre funciones, consulta estos enlaces:

- Referencia oficial de funciones de Arduino: https://www.arduino.cc/reference/en/language/functions/
- Aprende más sobre el principio DRY: https://es.wikipedia.org/wiki/No_te_repitas
- Un repaso sobre la historia de las subrutinas: https://es.wikipedia.org/wiki/Subrutina
