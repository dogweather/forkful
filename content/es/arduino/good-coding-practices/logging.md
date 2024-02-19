---
aliases:
- /es/arduino/logging/
date: 2024-01-26 00:58:29.919223-07:00
description: "\"Registrar\" es mantener un registro de eventos, transacciones o actividades\
  \ que suceden con el tiempo en un sistema. Los programadores lo utilizan para\u2026"
lastmod: 2024-02-18 23:09:10.267811
model: gpt-4-1106-preview
summary: "\"Registrar\" es mantener un registro de eventos, transacciones o actividades\
  \ que suceden con el tiempo en un sistema. Los programadores lo utilizan para\u2026"
title: "Registro de Actividades en Programaci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
"Registrar" es mantener un registro de eventos, transacciones o actividades que suceden con el tiempo en un sistema. Los programadores lo utilizan para depurar, monitorear la salud del sistema, recopilar estadísticas o incluso auditar el uso, lo que lo convierte en una práctica indispensable para mantener y entender el comportamiento de su código bajo diversas condiciones.

## Cómo hacerlo:
Arduino no viene con una biblioteca de registro integrada como algunos otros entornos, pero puedes implementar un registro básico en la consola Serial con un mínimo esfuerzo. Aquí tienes un ejemplo rápido para comenzar:

```arduino
void setup() {
  // Iniciar la comunicación serial con la tasa de baudios dada
  Serial.begin(9600);

  // Esperar a que el puerto serial se conecte - solo es necesario en algunas placas
  while (!Serial) {
    ; // esperar a que se conecte el puerto serial. Necesario para USB nativo
  }

  // Registrar un mensaje informativo indicando que el proceso de configuración está completo
  Serial.println("¡Configuración completa!");
}

void loop() {
  // Registrador simple que imprime el tiempo de funcionamiento cada segundo
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Tiempo de funcionamiento (ms): ");
    Serial.println(currentMillis);

    // Aquí también podrías añadir registros de errores, advertencias u otra información.
  }
  
  // Resto de la lógica de tu programa aquí...
}
```

Salida de la consola Serial:
```
¡Configuración completa!
Tiempo de funcionamiento (ms): 1000
Tiempo de funcionamiento (ms): 2000
Tiempo de funcionamiento (ms): 3000
...
```

## Estudio en Profundidad:
Históricamente, registrar en microcontroladores no era tan sencillo como en un sistema operativo completo. Los recursos limitados significaban que cada byte contaba, y los desarrolladores tenían que tener cuidado de no saturar el sistema. Con la llegada de placas más capaces y la plataforma Arduino simplificando el proceso, el registro se ha vuelto más accesible.

Mientras que el código anterior demuestra el registro a través de la interfaz Serial, otros métodos incluyen la escritura en una tarjeta SD, enviar datos a través de la red a un servidor remoto, o incluso la salida a una pequeña pantalla LCD.

La implementación de un sistema de registro conlleva consideraciones como la rotación, la severidad del nivel (información, depuración, advertencia, error) y el impacto en el rendimiento. En un Arduino, puede que necesites tener en cuenta las limitaciones de memoria al registrar estructuras de datos complejas. Para el registro remoto, la seguridad de los registros transmitidos también es una preocupación.

Existen soluciones más sofisticadas como Syslog, un estándar de registro ampliamente adoptado, fuera del mundo de Arduino, pero puedes integrar bibliotecas de terceros que ofrecen funcionalidad similar con varios grados de complejidad y requisitos de recursos.

## Ver También:
- [Referencia de `Serial` de Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Registro en tarjeta SD con Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Escudo de registro de datos de SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: Un ejemplo práctico de registro remoto con Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
