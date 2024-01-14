---
title:    "Arduino: Escribiendo pruebas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por qué es importante escribir pruebas en Arduino

Escribir pruebas es una parte esencial en la programación de Arduino. Esto permite asegurarse de que el código que se está escribiendo funciona correctamente y es libre de errores. Al escribir pruebas, se pueden identificar y solucionar problemas antes de implementar el código en un prototipo o proyecto final.

## Cómo escribir pruebas en Arduino

Para escribir pruebas en Arduino, se debe seguir una estructura específica. Se pueden utilizar las funciones `setup()` y `loop()` para establecer el entorno de prueba y ejecutar las pruebas respectivamente. Dentro de la función `loop()`, se deben escribir las diferentes pruebas utilizando la función `assert()` para verificar que los resultados obtenidos sean los esperados.

A continuación, se presenta un ejemplo de cómo escribir pruebas para una función que calcula el promedio de una lista de números:

```Arduino
// Función que calcula el promedio de una lista de números
float promedio(int lista[], int n) {
  float suma = 0;
  for (int i = 0; i < n; i++) {
    suma += lista[i];
  }
  return suma / n;
}

// Función de prueba
void test_promedio() {
  int lista1[5] = {10, 20, 30, 40, 50};
  assert(promedio(lista1, 5) == 30); // Prueba 1
  int lista2[3] = {25, 50, 75};
  assert(promedio(lista2, 3) == 50); // Prueba 2
}

// Función principal
void loop() {
  test_promedio(); // Ejecutar la función de prueba
}
```

En este ejemplo, se crean dos listas con números y se utilizan en las pruebas. La función `assert()` comprueba si el resultado obtenido en la función `promedio()` es igual al valor esperado y si no es así, mostrará un mensaje de error en la consola de Arduino.

Se pueden escribir tantas pruebas como se deseen, asegurándose de cubrir todos los casos posibles para garantizar la eficiencia y fiabilidad del código.

## Profundizando en la escritura de pruebas

Para escribir pruebas más avanzadas, se pueden utilizar diferentes herramientas y técnicas como las librerías `ArduinoUnit` y `AUnit` que proporcionan una estructura más completa para escribir y ejecutar pruebas.

También es importante tener en cuenta la separación de responsabilidades al escribir pruebas, es decir, probar cada función o módulo por separado para facilitar la identificación y solución de problemas.

Además, es recomendable escribir pruebas desde el inicio del desarrollo y no dejarlas para el final, ya que esto permite detectar problemas de manera temprana y ahorrar tiempo en la depuración del código.

## Ver también

- [Librería ArduinoUnit](https://github.com/mmurdoch/arduinounit)
- [AUnit: Arduino Unit Testing Framework](https://github.com/bxparks/AUnit)