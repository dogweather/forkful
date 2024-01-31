---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Escribir pruebas en programación significa crear pequeñas verificaciones para tu código. Los programadores las usan para asegurarse de que su código funciona correctamente y para evitar futuros errores tras cambios o actualizaciones.

## Cómo hacerlo:
No hay un framework de pruebas unitarias incluido en el entorno de desarrollo de Arduino, pero puedes hacer pruebas manuales con código simple. Por ejemplo, para probar una función que suma dos números:

```arduino
void setup() {
  Serial.begin(9600);
  int resultado = sumar(5, 3);
  Serial.print("Resultado: ");
  Serial.println(resultado);
}

void loop() {
  // Aquí va el código que queremos ejecutar continuamente.
}

int sumar(int a, int b) {
  return a + b;
}
```

Salida esperada en el Serial Monitor:
```
Resultado: 8
```

## Profundizando
En el contexto histórico, Arduino no se diseñó con un enfoque en pruebas unitarias debido a la limitación de recursos en microcontroladores. Alternativas para pruebas más complejas incluyen simular el Arduino en un PC o usar librerías como `ArduinoUnit`. La implementación de pruebas requiere una mezcla de seguimiento manual y automatización cuidadosamente planificada para ser efectiva en sistemas embebidos.

## Ver también
- ArduinoUnit library: https://github.com/Arduino-CI/ArduinoUnit
- Información sobre cómo simular Arduino en tu computador: https://www.sites.google.com/site/unoardusim/home
- Fundamentos de pruebas de software para no iniciados: https://www.pluralsight.com/courses/software-testing-fundamentals
