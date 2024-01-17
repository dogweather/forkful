---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Swift: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

¡Hola programadores de Swift! ¿Alguna vez has necesitado que tu programa lea los argumentos de línea de comando? Pues, ¡estás en el lugar correcto! En este artículo te explicaré qué es la lectura de argumentos de línea de comando y por qué es útil para los programadores de Swift.

## ¿Qué & Por qué?
La lectura de argumentos de línea de comando es cuando un programa recoge y procesa los valores que el usuario ingresa al ejecutar el programa desde la línea de comando. Esto puede ser útil para personalizar la ejecución del programa, por ejemplo, al proporcionar diferentes opciones o parámetros. Los programadores lo hacen para proporcionar una mayor flexibilidad y funcionalidad a sus programas.

## Cómo:
Para leer los argumentos de línea de comando en Swift, utilizamos el objeto `CommandLine` del framework `Foundation`. Primero, importamos el framework en nuestro código y luego podemos acceder a los argumentos utilizando la propiedad `arguments` del objeto `CommandLine`. Aquí hay un ejemplo de código que imprime los argumentos ingresados ​​por el usuario en la línea de comando:

```Swift
import Foundation

let arguments = CommandLine.arguments
print(arguments)
```

Si ejecutamos este programa en la línea de comando con algunos argumentos, por ejemplo `swift leerArgumentos.swift argumento1 argumento2`, obtendremos el siguiente resultado:

```Swift
["leerArgumentos.swift", "argumento1", "argumento2"]
```

¡Así de fácil es leer los argumentos de línea de comando en Swift!

## Deep Dive:
La lectura de argumentos de línea de comando no es una característica específica de Swift, es común en otros lenguajes de programación también. Algunos programadores pueden optar por usar librerías externas para leer argumentos de línea de comando, como `Commander` o `CommandLineKit`. Sin embargo, con el objeto `CommandLine` de Swift, podemos manejar esta tarea de manera simple y eficiente sin la necesidad de utilizar librerías externas.

Cuando se accede a la propiedad `arguments` del objeto `CommandLine`, obtendremos una matriz de cadenas que contiene los argumentos ingresados ​​por el usuario, donde el primer elemento es siempre el nombre del archivo del programa. También es importante tener en cuenta que los argumentos siempre se leen como cadenas, por lo que si necesitamos convertirlos a otros tipos de datos, deberemos realizar la conversión manualmente.

## See Also:
Si deseas obtener más información sobre la lectura de argumentos de línea de comando en Swift, puedes consultar la documentación oficial de Apple aquí: https://developer.apple.com/documentation/foundation/commandline

También puedes explorar las diferentes librerías externas disponibles para manejar la lectura de argumentos de línea de comando en Swift, como `Commander` (https://github.com/kylef/Commander) y `CommandLineKit` (https://github.com/jatoben/CommandLine).

Espero que este artículo te haya sido útil y te ayude a comprender mejor cómo leer argumentos de línea de comando en Swift. ¡Hasta la próxima! 🚀