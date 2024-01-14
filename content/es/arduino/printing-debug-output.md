---
title:                "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por Qué

Cuando estás trabajando en un proyecto de Arduino, a veces puede ser difícil saber lo que realmente está sucediendo en el código. Por eso, imprimir la salida de depuración es una excelente manera de diagnosticar y solucionar problemas en tu programa.

## Cómo Hacerlo

Para imprimir la salida de depuración, debemos utilizar la función "Serial.print()" en Arduino. Esta función nos permite imprimir mensajes y valores en la consola de serie, que se puede leer a través de un monitor serial externo.

```
Arduino ejemplo de código:

void setup() {
Serial.begin(9600); //Iniciar la comunicación serie a una velocidad de 9600 baudios
}

void loop() {
int x = 5;
Serial.print("El valor de x es: "); //Imprimir un mensaje
Serial.println(x); //Imprimir el valor de la variable x y un salto de línea
delay(1000); // Esperar un segundo antes de repetir el bucle
}
```

La salida de depuración se mostrará en la consola del monitor serial como "El valor de x es: 5". Puedes utilizar "Serial.println()" para imprimir valores de diferentes tipos de datos, como números enteros, flotantes, caracteres y cadenas de texto.

## Profundizando

Además de imprimir mensajes y valores, la función "Serial.print()" también puede ser utilizada para realizar un seguimiento del flujo de tu programa. Puedes insertarla en diferentes partes de tu código para ver si el programa está pasando por esa sección o no.

También hay otros comandos que se pueden utilizar en conjunto con "Serial.print()", como "Serial.begin()", "Serial.end()", "Serial.available()" y "Serial.read()". Estos te permiten controlar la comunicación serie y recibir datos desde el puerto serial.

No olvides comentar las líneas de código que utilizas para imprimir la salida de depuración una vez que hayas solucionado tus problemas. Esto te ayudará a saber qué cambios has realizado en el código y a mantener un registro de tu progreso.

## Ver También

- [Documentación Oficial de Arduino] (https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial de Debugging con Serial en Arduino] (https://create.arduino.cc/projecthub/taifur/debugging-serial-communication-in-arduino-b9eac7)
- [Cómo Utilizar el Monitor Serial en Arduino] (https://www.instructables.com/How-To-Use-Arduino-Serial-Ports/)