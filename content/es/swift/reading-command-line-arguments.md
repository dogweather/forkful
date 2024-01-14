---
title:    "Swift: Interpretando argumentos de línea de comandos"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##Por qué

Los argumentos de línea de comando son una herramienta útil para cualquier programador de Swift. Pueden ayudarte a personalizar tu código y hacer que sea más interactivo y versátil.

##Cómo hacerlo

Para leer los argumentos de línea de comando, primero debes importar la biblioteca Foundation:

```Swift
import Foundation
```

Luego, puedes acceder a los argumentos a través de la propiedad `CommandLine.arguments`, que devuelve una matriz de cadenas con los argumentos ingresados ​​por el usuario. Por ejemplo, si ejecutas tu programa con los argumentos `swift myProgram.swift hello world`, `CommandLine.arguments` contendrá `[hello, world]`.

Aquí hay un ejemplo de código que imprime todos los argumentos ingresados ​​por el usuario:

```Swift
import Foundation

for argument in CommandLine.arguments {
    print(argument)
}
```

Si ejecutas este ejemplo con `swift CommandLineArgs.swift hello world`, verás la siguiente salida:

```
hello
world
```

##Profundizando

Existen diferentes formas de leer y utilizar los argumentos de línea de comando en tu código Swift. Por ejemplo, también puedes utilizar el método `CommandLine.option()` para especificar opciones específicas que pueden tener valores asociados. Además, existen bibliotecas externas que pueden ayudarte a manejar argumentos de línea de comando más complejos.

Explore diferentes opciones para encontrar la mejor manera de leer y utilizar los argumentos de línea de comando en sus proyectos.

##Ver también

- [Documentación oficial de Apple sobre argumentos de línea de comando en Swift](https://developer.apple.com/documentation/foundation/commandline)
- [Ejemplo de repositorio de GitHub para leer argumentos de línea de comando en Swift](https://github.com/InfiniteLoopDK/CommandLineTool)
- [Artículo de Medium sobre manejo de argumentos de línea de comando en Swift](https://medium.com/@TheAngryDarling/using-command-line-arguments-in-swift-df869e7d88f2)