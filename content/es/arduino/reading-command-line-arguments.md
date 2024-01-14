---
title:    "Arduino: Leyendo argumentos de línea de comandos"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
Los argumentos de línea de comando son una herramienta esencial en la programación de Arduino. Te permiten interactuar con tu código y modificar su comportamiento sin tener que reescribirlo cada vez. En esta publicación, te explicaremos por qué leer los argumentos de línea de comando es importante y cómo puedes hacerlo en tus proyectos de Arduino.

## Cómo 
Para leer los argumentos de línea de comando en Arduino, primero debes incluir la biblioteca `commandline.h` en tu código. Luego, debes declarar una variable de tipo `CommandLine` y llamarla con el tamaño máximo de los argumentos que esperas recibir. A continuación, puedes utilizar la función `getArgument()` para obtener los argumentos introducidos por el usuario. Veamos un ejemplo de código:

```Arduino
#include <commandline.h>

void setup() {
  Serial.begin(9600);
  //crear una variable de tipo CommandLine con un máximo de 5 argumentos
  CommandLine cmd(5);
  //obtener el primer argumento introducido por el usuario
  String arg1 = cmd.getArgument(0);
  //imprimir el argumento en el monitor serial
  Serial.println(arg1);
}

void loop() {
  //tu código aquí
}
```

Si ejecutas este código y en la ventana de monitor serial de Arduino introduces "Hola mundo", verás que el código imprimirá "Hola" en el monitor serial. Esto se debe a que "Hola" es el primer argumento introducido, que se encuentra en la posición 0 de la variable `CommandLine`. Puedes seguir este mismo proceso para obtener y utilizar los argumentos que necesites en tus proyectos.

## Deep Dive
Los argumentos de línea de comando son útiles para muchas cosas, desde cambiar la configuración de un proyecto hasta proporcionar datos para su procesamiento. También puedes utilizarlos para crear opciones de ejecución en tu código, es decir, diferentes escenarios o acciones que se pueden activar o desactivar según los argumentos introducidos por el usuario. Por ejemplo, si tu proyecto tiene diferentes modos de funcionamiento, puedes utilizar un argumento para seleccionar el modo que deseas utilizar.

Un truco útil al leer los argumentos de línea de comando es utilizar la función `hasOption()`. Con esta función, puedes comprobar si un argumento determinado está presente o no. Por ejemplo:

```Arduino
if (cmd.hasOption("-a")){
  //haz algo en caso de que el argumento -a esté presente
}
```

Y para obtener varios argumentos en una sola línea, puedes utilizar un bucle `for`:

```Arduino
for (int i = 0; i < cmd.getArgumentCount(); i++) {
  String arg = cmd.getArgument(i);
  //haz algo con cada argumento obtenido
}
```

## Ver también
- [Documentación oficial de la biblioteca commandline] (https://github.com/ssp509/commandline)
- [Tutorial de programación de Arduino para principiantes] (https://maker.pro/arduino/tutorial/beginner-tips-arduino-command-line-arguments)