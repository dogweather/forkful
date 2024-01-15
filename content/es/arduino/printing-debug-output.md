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

## ¿Por qué imprimir mensajes de depuración?

Por lo general, cuando se programa con Arduino, se desea asegurarse de que el código esté funcionando correctamente y sin errores. Imprimir mensajes de depuración en la salida serial es una forma sencilla de monitorear el comportamiento del código y encontrar posibles problemas.

## Cómo hacerlo

La función `Serial.println()` es la forma más común de imprimir mensajes de depuración en Arduino. Simplemente se debe incluir el contenido que se desea imprimir dentro de los paréntesis. Por ejemplo:

```Arduino
int valor = 5;
Serial.println("El valor es: " + String(valor));
```

El resultado de este código sería el siguiente:

```
El valor es: 5
```

También se puede utilizar la función `Serial.print()` para imprimir sin una nueva línea al final. Además, es posible imprimir variables y datos de diferentes tipos, como números enteros, flotantes, caracteres y cadenas de texto.

## Buceo profundo

Es importante tener en cuenta que imprimir mensajes de depuración puede afectar el funcionamiento del código y ralentizar la velocidad de ejecución. Por lo tanto, es recomendable utilizar estas funciones solo durante la fase de desarrollo y desactivarlas en la versión final del programa.

Además, es posible agregar niveles de depuración en el código para imprimir mensajes solo cuando sea necesario. Esto se puede lograr utilizando condicionales y cambiando el valor de una variable que controle el nivel de depuración.

Otra opción útil es utilizar un monitor serial externo, como PuTTY, para visualizar los mensajes de depuración. Esto permite una mejor organización y visualización de los datos de salida.

## Ver también

- [Documentación oficial de Arduino sobre la función Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Tutorial de Arduino en español sobre cómo imprimir mensajes de depuración](https://www.luisllamas.es/imprimir-mensajes-de-depuracion-en-arduino-serial-print/)
- [Vídeo tutorial en español sobre cómo utilizar PuTTY para imprimir mensajes de depuración](https://youtu.be/aEACiPoBJNo)