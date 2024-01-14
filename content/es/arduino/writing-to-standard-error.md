---
title:    "Arduino: Escribiendo en el error estándar"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de error estándar en Arduino?

La escritura a la salida de error estándar en Arduino es una herramienta útil para depurar y solucionar problemas en programas. Al imprimir mensajes de error en la salida estándar, podemos identificar fácilmente dónde y por qué ocurre un error en nuestro código.

## Cómo hacerlo

Para escribir a la salida de error estándar en Arduino, debemos utilizar la función `Serial.print()`, especificando el número del puerto serie en el que queremos imprimir. Por ejemplo, para imprimir en la salida de error estándar, utilizamos `Serial.print("Mensaje de error", Serial);`.

Un ejemplo de código que utiliza la escritura a la salida de error estándar puede ser el siguiente:

```Arduino
void setup() {
  Serial.begin(9600); // Inicializar comunicación con el puerto serie
}

void loop() {
  // Realizar alguna operación que pueda producir un error
  int resultado = 10 / 0;
  
  // Si ocurre un error, escribir un mensaje a la salida de error estándar
  if (resultado == NULL) {
    Serial.print("Error: No se puede dividir por cero", Serial); 
  }
}
```

La salida en el monitor serie se vería así:

```
Error: No se puede dividir por cero
```

## Inmersión profunda

Al utilizar la escritura a la salida de error estándar, es importante tener en cuenta que esta función imprime directamente en el puerto serie seleccionado, sin formato ni procesamiento del mensaje. Esto significa que si nuestro mensaje incluye variables, debemos convertirlas a una cadena de caracteres antes de imprimirlas.

Además, es importante recordar que la salida a la pantalla puede retrasar la ejecución del programa, por lo que debemos utilizarla con precaución y solo en caso de necesidad.

## Ver también

- [Documentación de Arduino sobre la función Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial de Arduino sobre depuración y manejo de errores](https://www.arduino.cc/en/Tutorial/ArduinoDebugging)
- [Ejemplos de código utilizando la función Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/examples/)