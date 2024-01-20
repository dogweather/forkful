---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas es una forma de garantizar que nuestro código funcione correctamente y se comporte según lo esperado. Además, nos permite detectar posibles errores y solucionarlos antes de que nuestro programa se ponga en funcionamiento.

## Cómo:

Para escribir pruebas en Arduino, podemos utilizar algunas librerías disponibles como "ArduinoUnit" o "UnitTest". También podemos crear nuestras propias pruebas siguiendo los siguientes pasos:

1. Crear una función de prueba utilizando la palabra clave "void" seguida del nombre de la prueba.
```Arduino
void miPrueba(){
  //código de la prueba
}
```
2. Utilizar la función "assert" para comprobar si nuestra prueba ha sido exitosa.
```Arduino
void miPrueba(){
  assert(3 + 2 == 5);  //si la suma es igual a 5, la prueba es exitosa
}
```
3. Ejecutar la prueba utilizando la función "run" en el "setup" de nuestro código.
```Arduino
void setup(){
  run(miPrueba);  //ejecutamos nuestra prueba
}
```
4. Verificar el resultado en el monitor serie.
```Arduino
void setup(){
  run(miPrueba);  //ejecutamos nuestra prueba
  Serial.println("Prueba exitosa");  //si pasó la prueba, se mostrará en el monitor
}
```

## Deep Dive:

El uso de pruebas en programación es una técnica muy común que permite asegurar la calidad del código y prevenir errores en el futuro. Entre las alternativas a las librerías mencionadas anteriormente, podemos mencionar "CppUTest" o "Google Test", que ofrecen una amplia gama de funcionalidades adicionales para escribir y ejecutar pruebas.

En cuanto a la implementación, es importante tener en cuenta que las pruebas deben ser independientes y no afectar al funcionamiento del código principal. Además, se recomienda escribir pruebas para diferentes partes del código, cubriendo la mayor cantidad de casos posibles.

## See Also:

- [Documentación de ArduinoUnit](https://github.com/mmurdoch/arduinounit)
- [Documentación de CppUTest](https://cpputest.github.io/)
- [Documentación de Google Test](https://github.com/google/googletest)