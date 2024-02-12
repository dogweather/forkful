---
title:                "Escribiendo pruebas"
aliases:
- /es/arduino/writing-tests.md
date:                  2024-02-03T19:29:29.588196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en el entorno de Arduino se refiere al proceso de crear pruebas automatizadas que validan la funcionalidad de tu código en dispositivos Arduino. Los programadores lo hacen para asegurarse de que su código funciona como se espera, reduce errores y mejora la calidad de sus proyectos, especialmente crucial en sistemas embebidos donde la depuración puede ser más desafiante.

## Cómo:

Arduino no tiene un marco de pruebas integrado como algunos otros entornos de programación. Sin embargo, puedes usar bibliotecas de terceros como `AUnit` para la prueba unitaria del código Arduino. AUnit está inspirado en la biblioteca integrada de Arduino, `ArduinoUnit`, y el marco de pruebas de Google, `Google Test`.

### Ejemplo con AUnit:

Primero, instala AUnit a través del Administrador de Bibliotecas en el IDE de Arduino: ve a Sketch > Incluir Biblioteca > Administrar Bibliotecas... > busca AUnit e instálalo.

Luego, puedes escribir pruebas de la siguiente manera:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, SALIDA);
  digitalWrite(ledPin, ALTO);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, SALIDA);
  digitalWrite(ledPin, BAJO);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Vacío
}
```
Después de subir esta prueba a tu placa Arduino, abre el Monitor Serial para ver los resultados de la prueba. Deberías ver una salida que indica si cada prueba pasó o falló:

```
TestRunner comenzó en 2 prueba(s).
Test ledPinHigh pasó.
Test ledPinLow pasó.
Duración de TestRunner: 0.002 segundos.
Resumen de TestRunner: 2 pasaron, 0 fallaron, 0 omitidos, 0 agotaron el tiempo, de un total de 2 prueba(s).
```

Este ejemplo simple demuestra el uso de AUnit para probar el estado de un pin de LED. Al crear pruebas, confirmas que tu Arduino se comporta como se espera en diferentes condiciones. Con AUnit, puedes escribir pruebas más complejas, suites de pruebas y disfrutar de características como tiempos de espera de pruebas y procedimientos de configuración/desmontaje para escenarios más avanzados.
