---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Empezar un nuevo proyecto de Arduino significa crear un conjunto de instrucciones en un programa para un Arduino board. Los programadores hacen esto para transformar el Arduino en una herramienta adaptada a sus necesidades específicas, ya sea un robot, un monitor de temperatura u otros dispositivos automatizados.

## Cómo Hacer:

Aquí os muestro un ejemplo de código y su salida correspondiente. Estamos usando un LED conectado al pin digital 13.

```Arduino 
void setup(){
  pinMode(13, OUTPUT); 
}

void loop(){
  digitalWrite(13, HIGH);
  delay(1000);
  digitalWrite(13, LOW);
  delay(1000);
}
```

Esta salida hace que el LED parpadee cada segundo.

## Profundización:

El Arduino surgió en 2005 como una herramienta para estudiantes sin experiencia en electrónica o microcontroladores. Ahora se utiliza a nivel mundial en muchos campos, desde la educación hasta la robótica. Existen alternativas como Raspberry Pi, pero Arduino sigue siendo la opción más sencilla para principiantes. Iniciar un nuevo proyecto consiste sobre todo en definir la configuración inicial (`setup()`) y especificar las tareas que se repiten (`loop()`).

## Ver También:

Para aquellos que estén buscando un poco más de ayuda o dosis extra de conocimientos, aquí os dejo algunos enlaces:

- Tutorials de Arduino: https://www.arduino.cc/en/Tutorial/HomePage
- Foro de Arduino: https://forum.arduino.cc/
- Referencias de Arduino: https://www.arduino.cc/reference/en/