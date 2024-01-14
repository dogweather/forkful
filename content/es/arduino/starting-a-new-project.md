---
title:    "Arduino: Comenzando un nuevo proyecto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando una manera emocionante y práctica de explorar la electrónica y la programación, ¡entonces Arduino es la respuesta! Con Arduino, puedes crear una amplia gama de proyectos, desde robots hasta sistemas de automatización del hogar. ¡Así que comencemos a explorar cómo puedes dar tus primeros pasos en el mundo de Arduino!

## Cómo hacerlo

Para empezar, necesitarás un Arduino y un cable USB para conectarlo a tu computadora. Descarga el software de Arduino en tu computadora y asegúrate de que tu placa esté correctamente conectada. Ahora, puedes escribir tu primer programa en el IDE de Arduino.

```Arduino 

void setup() {
  // Este código se ejecuta solo una vez
  pinMode(LED_BUILTIN, OUTPUT); // Configura el pin 13 como salida
}

void loop() {
  // Este código se ejecuta repetidamente
  digitalWrite(LED_BUILTIN, HIGH); // Enciende el LED
  delay(1000); // Espera 1 segundo
  digitalWrite(LED_BUILTIN, LOW); // Apaga el LED
  delay(1000); // Espera 1 segundo
}
```

Este es un programa simple que enciende y apaga un LED conectado al pin 13 cada segundo. Una vez que hayas escrito tu código, puedes subirlo a tu Arduino haciendo clic en el botón "Subir" en la barra de herramientas.

## Inmersión profunda

Para poner en marcha un proyecto de Arduino, es importante tener una comprensión básica de la programación y la electrónica. Afortunadamente, hay una gran cantidad de recursos disponibles en línea, desde manuales de inicio hasta tutoriales y proyectos completos para inspirarte. También hay comunidades en línea activas donde puedes hacer preguntas y obtener ayuda de otros usuarios de Arduino.

Al comenzar un proyecto, es importante tener claro el objetivo y planificar antes de ponerse a trabajar. Esto incluye decidir qué componentes necesitarás, cómo estarán conectados y cómo funcionará el programa. Es una buena idea probar y ajustar tu código a medida que avanzas en tu proyecto para asegurarte de que funcione correctamente.

## Ver también

- [Manual de inicio de Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [Tutoriales de Arduino](https://www.arduino.cc/en/Tutorial/HomePage)
- [Proyectos de Arduino](https://create.arduino.cc/projecthub)
- [Comunidad de Arduino en español](https://www.arduino.cc/es/)
- [Foro de Arduino](https://forum.arduino.cc/index.php?board=1.0)