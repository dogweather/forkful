---
title:                "Usando un depurador"
date:                  2024-01-26T04:10:28.961829-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Usar un depurador significa aprovechar herramientas especializadas para probar e inspeccionar tu código mientras se ejecuta. Es importante porque te permite ver qué está sucediendo bajo el capó, encontrar errores y entender mejor el comportamiento de tu código.

## Cómo hacerlo:
Para usar el depurador en Xcode (el IDE para Swift), puedes establecer puntos de interrupción, inspeccionar variables y observar expresiones. Aquí tienes un ejemplo:

```Swift
func findFactorial(de number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(de: number - 1)
}

let result = findFactorial(de: 5)
print(result)
```

Establece un punto de interrupción haciendo clic a la izquierda de un número de línea en Xcode y ejecuta el programa. Cuando llegue al punto de interrupción, Xcode pausará la ejecución. Ahora puedes:

1. Verificar los valores de las variables.
2. Avanzar sobre (ejecutar la próxima línea) o entrar en (ir dentro de una función) usando los controles del depurador.
3. Agregar expresiones a la 'lista de vigilancia' para monitorear cambios en variables o constantes específicas.

Esto es lo que podrías ver en el área de depuración:

```
(lldb) po number
5
(lldb) po result
120
```

## Profundización:
Los depuradores han sido parte del paisaje de programación desde los años 40, evolucionando desde sistemas simples de puntos de interrupción hasta experiencias complejas, impulsadas por UI. Otras opciones además del depurador integrado de Xcode incluyen herramientas de terceros como LLDB (Low Level Debugger), que Xcode utiliza bajo el capó. Algunas personas incluso depuran con declaraciones de `print()` (conocido afectuosamente como "debugging de cavernícola"), pero esto es menos eficiente para proyectos grandes o errores complejos. Cuando usas un depurador, estás manejando el control de ejecución, la introspección en tiempo de ejecución y la manipulación de datos. Un profundo entendimiento de estos principios ayuda mucho en la depuración eficiente.

## Ver También:
- [Guía de Depuración de Xcode de Apple](https://developer.apple.com/documentation/xcode/debugging-your-app)
- [Guía Rápida de Inicio de LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Tutorial de Depuración de Swift de Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
