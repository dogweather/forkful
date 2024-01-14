---
title:    "Arduino: Leyendo argumentos de línea de comandos"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por qué leer argumentos de línea de comando en Arduino

Si eres un entusiasta de la programación y estás interesado en aprender Arduino, es importante que tengas conocimientos sobre cómo leer argumentos de línea de comando. Esto te permitirá controlar tu Arduino de una manera más eficiente y personalizada.

## Cómo hacerlo
Para leer argumentos de línea de comando en Arduino, necesitarás utilizar una función llamada ```Serial.readString()```. Esta función te permite leer datos de entrada desde el puerto serial, lo que significa que puedes enviar comandos a tu Arduino a través de un terminal o programa de terminal como PuTTY o CoolTerm.

Aquí te dejamos un ejemplo de código que puedes probar en tu Arduino:

```
void setup() {
  // Inicializar el puerto serial a una velocidad de 9600
  Serial.begin(9600);
}

void loop() {
  // Leer el comando ingresado por el usuario desde el puerto serial
  String command = Serial.readString();

  // Imprimir el comando en el monitor serial
  Serial.println("Comando recibido: " + command);

  // Esperar 1 segundo antes de volver a leer
  delay(1000);
}
```

Si abres el monitor serial y envías un comando como "Hola", deberías ver el siguiente resultado:

```
Comando recibido: Hola
```

Ahora puedes experimentar con diferentes comandos y hacer que tu Arduino realice diferentes acciones en función de ellos.

## Profundizando un poco más
La lectura de argumentos de línea de comando te permite interactuar con tu Arduino en tiempo real, pero también es una habilidad útil para aplicaciones más avanzadas. Por ejemplo, puedes utilizarla para configurar y ajustar parámetros de tu Arduino sin tener que reconfigurar el código cada vez.

Además, la función ```Serial.readString()``` también te permite especificar un delimitador para que el programa sepa dónde termina un comando y comienza el siguiente. Esto es especialmente útil si estás enviando múltiples comandos en una sola entrada.

## Véase también
- [Documentación de Arduino sobre la función Serial.readString()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/) 
- [Tutorial de Arduino sobre la lectura de comandos de entrada](https://www.arduino.cc/en/Tutorial/ReadASCIIString) 
- [Página de CoolTerm para descargar el programa de terminal CoolTerm](http://freeware.the-meiers.org/)