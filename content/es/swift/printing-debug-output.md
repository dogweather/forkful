---
title:                "Swift: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Imprimir mensajes de depuración es una práctica común en el desarrollo de aplicaciones. Es útil para identificar errores y entender el flujo de ejecución de un programa.

## Cómo hacerlo
Para imprimir mensajes de depuración en Swift, se puede utilizar la función `print()`. Por ejemplo:

```Swift 
let nombre = "Juan"
print("El nombre es \(nombre)")
```

Esto imprimirá en la consola el siguiente mensaje: `El nombre es Juan`. 

También se puede imprimir el contenido de una variable o constante sin usar el formato de cadena. Por ejemplo:

```Swift 
let edad = 25
print(edad)
```

Esto imprimirá el valor de la variable `edad`, en este caso `25`.

Otra forma de imprimir mensajes de depuración es utilizando el comando `debugPrint()`. Este comando imprimirá el valor de una variable o constante en un formato más legible. Por ejemplo:

```Swift 
let lista = ["manzana", "naranja", "plátano"]
debugPrint(lista)
```

La salida en la consola será la siguiente:

```
["manzana", "naranja", "plátano"]
```

## Profundizando
Además de imprimir mensajes de depuración simples, también se pueden usar comandos especiales para obtener información más detallada. Por ejemplo, el comando `dump()` imprimirá la estructura y los valores de un objeto completo. 

```Swift 
let precio = 12.5
dump(precio)
```

La salida en la consola será la siguiente:

```
▿ 12.5
  - some: 12.5
```

Además, se pueden agregar mensajes personalizados a los comandos de depuración para hacer más fácil la identificación de errores. Por ejemplo:

```Swift 
let numero = 5
print("El número es: \(numero)", separator: " - ", terminator: "!")
```

La salida en la consola será la siguiente:

```
El número es: 5!
```

## Ver también
Para obtener más información sobre el uso de mensajes de depuración en Swift, puedes consultar los siguientes enlaces:

- [Guía oficial de Apple sobre la depuración en Swift](https://developer.apple.com/library/archive/documentation/ToolsLanguages/Conceptual/Xcode_Overview/DebugYourApp/DebugYourApp.html)
- [Tutorial de depuración en Swift de Hacking with Swift](https://www.hackingwithswift.com/read/17/overview)
- [Artículo sobre el uso de mensajes de depuración en Swift de Ray Wenderlich](https://www.raywenderlich.com/773-how-to-debug-with-print-statements-in-swift)

¡Esperamos que este artículo te haya sido útil en tu aprendizaje de Swift! ¡No dudes en utilizar mensajes de depuración en tu próximo proyecto para facilitar tu proceso de desarrollo!