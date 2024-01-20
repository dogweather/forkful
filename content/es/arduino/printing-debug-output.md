---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Imprimir datos de depuración consiste en desplegar la información útil en un terminal durante la ejecución de un código. Los programadores lo hacen para diagnosticar los problemas en sus programas.

## Cómo hacerlo:

```Arduino
void setup() {
  Serial.begin(9600);   //Iniciamos la comunicación serial a 9600 baud
}

void loop() {
  Serial.println("¡Hola, Mundo!");   //Imprimimos el mensaje
  delay(1000);    //Pausamos por 1 segundo
}
```

El bloque de código anterior inicia la comunicación serial a 9600 baud en la función setup() y luego, en un bucle infinito, imprime "¡Hola, Mundo!" en la terminal serie cada segundo.

## Profundizando

La depuración es una parte esencial de la programación desde sus inicios. Los programadores solían depurar imprimiendo el estado de su programa en tarjetas perforadas. Hoy en día, la impresión de datos por el puerto serial es la forma más común de depuración en sistemas Arduino.

Existen alternativas a esta técnica, como la depuración paso a paso, otras bibliotecas de depuración e incluso osciloscopios. Sin embargo, la impresión de la salida de depuración es fácil de implementar y de aprender para los principiantes.

La implementación del debug en Arduino es bastante directa gracias a la biblioteca Serial. Puedes elegir la velocidad de comunicación (en baud), imprimir cualquier cosa que necesites y luego mostrarla con el Monitor Serial integrado en el IDE de Arduino.

## Ver También

- [Documentación oficial de Arduino en la biblioteca Serial (en inglés)](https://www.arduino.cc/en/Reference/Serial)