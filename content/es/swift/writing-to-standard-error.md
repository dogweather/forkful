---
title:    "Swift: Escribiendo a la salida de error estándar"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

Hay muchas razones por las que uno podría querer escribir al error estándar en Swift. Una de las razones más comunes es la depuración de código, ya que escribir al error estándar puede ayudar a encontrar errores y problemas en el código.

## Cómo hacerlo

Para escribir al error estándar en Swift, puedes usar la función `print(_:to:)` y pasarle `Stderr` como destino.

```Swift
print("Este es un mensaje al error estándar", to: &Stderr)
```

El resultado se mostrará en la consola de errores en lugar de la consola de salida estándar.

<sub><sup>**Salida:** Este es un mensaje al error estándar</sup></sub>

## Profundizando

Además de la depuración, escribir al error estándar en Swift también puede ser útil para registrar errores y excepciones en tu código. Puedes utilizar la instrucción `fatalError` para generar un mensaje de error y escribirlo al error estándar al mismo tiempo. También puedes personalizar tu propia función de registro de errores para escribir al error estándar.

```Swift
// Función personalizada para registrar errores
func logError(_ error: String) {
    print("ERROR: \(error)", to: &Stderr)
}

// Ejemplo de uso
logError("Etiqueta de error")
```

Recuerda que escribir al error estándar no es lo mismo que lanzar una excepción. Escribir al error estándar simplemente imprime un mensaje en la consola, mientras que lanzar una excepción detiene la ejecución del programa. Por lo tanto, debes elegir la mejor opción según tus necesidades.

## Ver también

- [Documentación oficial de Swift sobre la escritura al error estándar](https://developer.apple.com/documentation/swift/erroroutput)
- [Tutorial de depuración de código en Swift](https://www.raywenderlich.com/470-how-to-debug-in-swift)
- [Guía de excepciones y errores en Swift](https://www.swiftbysundell.com/posts/throwing-observers-in-swift)