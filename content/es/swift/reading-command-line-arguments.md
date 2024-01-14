---
title:                "Swift: Leyendo argumentos de línea de comandos."
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Los argumentos de línea de comando son una forma útil de interactuar con nuestro programa mientras se ejecuta, ya sea para proporcionar información o tomar decisiones en tiempo real. En esta publicación, veremos cómo leer y utilizar los argumentos de línea de comando en Swift para hacer que nuestros programas sean más dinámicos y versátiles.

## Cómo hacerlo

Para leer argumentos de línea de comando en Swift, podemos utilizar la clase `CommandLine`, que nos proporciona una matriz de cadenas con los argumentos pasados al programa. Veamos un ejemplo de cómo leer y utilizar estos argumentos:

```Swift
let argumentos = CommandLine.arguments
print("Cantidad de argumentos: \(argumentos.count)")

// Si se proporciona un argumento adicional, lo usamos para dar un saludo personalizado
if argumentos.count > 1 {
    let nombre = argumentos[1]
    print("¡Hola, \(nombre)! Bienvenido a nuestro programa.")
}
```

Supongamos que ejecutamos nuestro programa con los argumentos `swift miPrograma.swift John`:
```
Cantidad de argumentos: 2
¡Hola, John! Bienvenido a nuestro programa.
```

## Profundizando

Aparte de la cantidad y los valores de los argumentos, también podemos acceder a otras propiedades de la clase `CommandLine`, como el nombre del ejecutable, opciones y argumentos de opción específicos. Por ejemplo, supongamos que nuestro programa tiene una opción `-verbose` que nos permite imprimir información adicional. Podríamos implementarlo de la siguiente manera:

```Swift
let argumentos = CommandLine.arguments
if argumentos.contains("-verbose") {
    print("Opción verbose activada.")
}
```

De esta manera, podemos leer y utilizar los argumentos de línea de comando para personalizar la ejecución de nuestro programa y proporcionar una mejor experiencia para nuestros usuarios.

## Ver también

Para más información sobre cómo utilizar argumentos de línea de comando en Swift, puedes consultar la documentación oficial de Apple: [Lectura y uso de argumentos de la línea de comando](https://developer.apple.com/documentation/foundation/commandline). También tenemos una publicación sobre cómo crear un programa simple utilizando argumentos de línea de comando en Swift: [Cómo hacer un Saludo Personalizado Utilizando Argumentos de Línea de Comando en Swift](https://www.example.com/).