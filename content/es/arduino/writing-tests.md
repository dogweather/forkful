---
title:    "Arduino: Escribir pruebas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Arduino

Escribir pruebas al programar en Arduino es una práctica esencial para garantizar la funcionalidad y fiabilidad de nuestros proyectos. Además, nos permite identificar y corregir errores de manera eficiente, ahorrando tiempo y evitando problemas en el futuro.

## Cómo escribir pruebas en Arduino

Para escribir pruebas en Arduino, podemos seguir los siguientes pasos:

1. Identificar las funciones o partes del código que queremos probar.
2. Crear un sketch de prueba separado del código principal.
3. Agregar una función de prueba que llame a la función o parte del código que queremos probar.
4. Utilizar la función `assert()` para verificar si la salida de la función de prueba es la esperada.
5. Ejecutar el sketch de prueba y verificar los resultados.

Un ejemplo de cómo escribir y ejecutar una prueba en Arduino sería el siguiente:

```Arduino
void setup() {

}

void loop() {
  assert(square(5) == 25);
}

int square(int num) {
  return num * num;
}
```

En este caso, estamos probando la función `square()` para asegurarnos de que el resultado sea el esperado. Si ejecutamos el sketch de prueba y no obtenemos errores, significa que la función está funcionando correctamente.

## Profundizando en la escritura de pruebas

Escribir pruebas en Arduino no solo nos ayuda a identificar y corregir errores, sino que también nos ayuda a mantener un código más limpio y organizado. Al crear pruebas para cada función o parte del código, podemos comprobar fácilmente si algún cambio que hacemos afecta el funcionamiento de otras partes.

Además, existen herramientas y bibliotecas disponibles que facilitan la escritura y ejecución de pruebas en Arduino, como la biblioteca "Arduino Unit" o el framework de pruebas "Unity for Arduino". Estas herramientas ofrecen una amplia gama de funciones y asistentes que facilitan la creación y ejecución de pruebas.

# Ver también

- Blog de Arduino: https://blog.arduino.cc/
- Documentación de Arduino Unit: http://arduinounit.github.io/
- Tutorial de Unity for Arduino: https://www.instructables.com/id/Unit-Testing-for-Arduino/