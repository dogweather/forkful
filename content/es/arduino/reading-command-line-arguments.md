---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Arduino: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

¡Hola a todos! ¡Bienvenidos a mi artículo sobre cómo leer argumentos de línea de comandos en Arduino! En este artículo, aprenderás qué es leer argumentos de línea de comandos y por qué es importante para los programadores. También te mostraré cómo hacerlo de manera sencilla. ¡Así que agarra tu Arduino y vamos a empezar!

## ¿Qué y por qué?

Leer argumentos de línea de comandos significa recibir información ingresada por el usuario en la línea de comandos de la interfaz de Arduino. Los programadores lo hacen para poder interactuar con su programa en tiempo de ejecución y modificar su comportamiento según la información proporcionada por el usuario. Esto puede ser especialmente útil en proyectos en los que es necesario ajustar ciertos parámetros o tomar acciones específicas a través de la entrada del usuario.

## ¿Cómo hacerlo?

Para leer argumentos de línea de comandos en Arduino, debemos utilizar el objeto ```Serial```. Primero, asegúrate de que la configuración de velocidad de tu serial monitor coincida con la velocidad que has configurado en el código (`Serial.begin()`). Luego, en la función `setup()`, agrega la línea `Serial.begin()` para inicializar la comunicación serial. Ahora, en la función `loop()`, podemos usar `Serial.read()` para leer los datos ingresados por el usuario a través de la interfaz serial. Veamos un ejemplo:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    char input = Serial.read();
    Serial.println(input);
  }
}
```

En este ejemplo, si ingresamos "hola" en la línea de comandos, la salida sería "hola". ¡Fácil, ¿verdad?

## Profundizando

Si bien leer argumentos de línea de comandos puede parecer una tarea simple, es importante entender cómo funciona para poder utilizarlo de manera efectiva. Alternativas a este método incluyen utilizar una pantalla LCD o un teclado matricial para ingresar la información del usuario. Sin embargo, esto requiere de componentes adicionales y podría no ser tan práctico en ciertos proyectos. En cuanto a la implementación, es importante tener en cuenta que `Serial.read()` devuelve un valor entero que representa el código ASCII del carácter ingresado. Por lo tanto, es necesario convertirlo al tipo de dato que deseamos utilizar en nuestro programa.

## Ver también

Si deseas aprender más sobre cómo interactuar con Arduino a través de la línea de comandos, échale un vistazo a estos recursos:

- [The Arduino Serial library reference](https://www.arduino.cc/en/Reference/Serial)
- [A beginner's guide to the Arduino Serial Monitor](https://randomnerdtutorials.com/arduino-serial-monitor/)
- [Arduino Projects Book](https://store.arduino.cc/products/arduino-starter-kit)