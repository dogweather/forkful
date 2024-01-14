---
title:    "Arduino: Extrayendo subcadenas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar la extracción de subcadenas en Arduino?

Extraer subcadenas de una cadena de texto puede ser una tarea útil en la programación de Arduino. Puede ser útil para analizar datos obtenidos a través de un sensor o para manipular datos de entrada de forma más eficiente. En este artículo, aprenderemos cómo utilizar la extracción de subcadenas en tus proyectos de Arduino.

## Cómo hacerlo

Para extraer una subcadena de una cadena de texto en Arduino, se pueden seguir los siguientes pasos:

1. Primero, declara la cadena de texto original y la subcadena deseada a través de la función `String`.

  ```Arduino
  String stringOriginal = "Hola mundo";
  String subcadena = "";
  ```

2. Luego, utiliza la función `substring()` para obtener una parte específica de la cadena original. Esta función toma dos argumentos: el índice de inicio y el índice de fin de la subcadena deseada.

  ```Arduino
  // Extraer "mundo" desde la posición 5 hasta el final
  subcadena = stringOriginal.substring(5);
  ```

3. Alternativamente, también puedes especificar el índice de inicio y el número de caracteres que se deben extraer.

  ```Arduino
  // Extraer "Hola" desde la posición 0 con una longitud de 4 caracteres
  subcadena = stringOriginal.substring(0, 4);
  ```

4. Por último, puedes utilizar la función `println()` para imprimir la subcadena resultante en el monitor serie de Arduino.

  ```Arduino
  // Imprimir subcadena en el monitor serie
  Serial.println(subcadena);
  ```

En el código anterior, reemplaza `Serial` con el nombre de tu puerto serie si es necesario.

## Profundizando

La extracción de subcadenas puede ser especialmente útil cuando se trabaja con datos en formato CSV (valores separados por comas). En este caso, puedes utilizar la función `indexOf()` para buscar la posición de una coma y extraer los valores entre las comas como subcadenas.

Por ejemplo, si tienes una cadena de texto en formato CSV como "Sensor1, Sensor2, Sensor3" y quieres extraer los valores individuales para guardarlos en variables separadas, puedes hacer lo siguiente:

```Arduino
// Declara la cadena de texto original y variables para guardar los valores
String valores = "Sensor1, Sensor2, Sensor3";
String valor1 = "";
String valor2 = "";
String valor3 = "";

// Extrae los valores individuales y guárdalos en variables
// utilizando la función indexOf() y substring()
valor1 = valores.substring(0, valores.indexOf(','));
valor2 = valores.substring(valores.indexOf(',') + 2, valores.lastIndexOf(','));
valor3 = valores.substring(valores.lastIndexOf(',') + 2);

// Imprime los valores en el monitor serie
Serial.println(valor1);
Serial.println(valor2);
Serial.println(valor3);
```

Este es solo un ejemplo de cómo se puede utilizar la extracción de subcadenas de forma más avanzada en tus proyectos de Arduino.

## Ver También

- [Documentación de Arduino sobre la función `substring()`](https://www.arduino.cc/reference/es/language/variables/data-types/string/functions/substring/)
- [Tutorial de Adafruit sobre la extracción de subcadenas en Arduino](https://learn.adafruit.com/arduino-tips-tricks-and-techniques/strings-and-text-with-arduino) 
- [Ejemplo de extracción de subcadenas en Arduino para analizar datos de un sensor](https://maker.pro/arduino/projects/arduino-dht11-temp-humidity-sensor-using-substring)