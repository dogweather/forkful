---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Arduino: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado cómo puedes hacer que tu Arduino sea más adaptable y configurable? Una forma de lograrlo es utilizando argumentos de línea de comandos. Al leer este artículo, aprenderás cómo usarlos y de qué manera pueden ayudarte en tus proyectos.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Arduino, necesitarás una librería llamada "ArgParse". Esta librería te permite definir los argumentos que deseas leer y obtener fácilmente su valor.

Debes comenzar descargando e instalando la librería ArgParse en tu IDE de Arduino. Luego, importa la librería en tu código, usando la instrucción ```#include <ArgParse.h>```.

A continuación, debes crear un objeto de tipo ArgumentParser, que será el encargado de gestionar los argumentos. Puedes hacerlo de la siguiente manera:

```Arduino
ArgumentParser parser;
```

Luego, deberás definir los argumentos que deseas leer. Puedes hacerlo usando la función ```addArgument()```, pasando como parámetro el nombre del argumento, el tipo de datos que esperas recibir y una descripción opcional. Por ejemplo:

```Arduino
parser.addArgument("led", STRING, "Indica el estado del LED");
```

Una vez que hayas definido todos los argumentos, es momento de leerlos. Puedes hacerlo usando la función ```parse()```, que analizará los argumentos ingresados y los almacenará en el objeto ArgumentParser. Por ejemplo:

```Arduino
parser.parse();
```

Finalmente, puedes obtener el valor de cada argumento usando la función ```get()```, pasando como parámetro el nombre del argumento. Por ejemplo:

```Arduino
String ledState = parser.get("led");
```

## Profundizando

La librería ArgParse también incluye otras funciones útiles, como la posibilidad de establecer valores predeterminados para los argumentos, manejar argumentos opcionales y validar los tipos de datos ingresados.

Además, si quieres saber más sobre cómo funcionan los argumentos de línea de comandos y cómo puedes utilizarlos en tus proyectos, te recomendamos que leas la documentación oficial de la librería ArgParse.

## Ver también

- [Documentación oficial de ArgParse](https://github.com/darkcommander/Arduino-ArgParse)
- [Tutorial sobre cómo leer argumentos de línea de comandos en Arduino](https://www.arduino.cc/en/tutorial/commandlineargs)
- [Ejemplos de uso de la librería ArgParse](https://github.com/darkcommander/Arduino-ArgParse/tree/master/examples)