---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:06.309425-07:00
description: "El manejo de errores en Go implica reconocer y responder a las condiciones\
  \ de error en tu programa. Los programadores se dedican al manejo de errores para\u2026"
lastmod: 2024-02-19 22:05:17.123074
model: gpt-4-0125-preview
summary: "El manejo de errores en Go implica reconocer y responder a las condiciones\
  \ de error en tu programa. Los programadores se dedican al manejo de errores para\u2026"
title: Manejando errores
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El manejo de errores en Go implica reconocer y responder a las condiciones de error en tu programa. Los programadores se dedican al manejo de errores para asegurar que sus aplicaciones puedan recuperarse grácilmente de situaciones inesperadas, lo que lleva a un software más robusto y confiable.

## Cómo hacerlo:

En Go, el manejo de errores se gestiona explícitamente mediante el tipo `error`. Las funciones que pueden fallar devuelven un error como su último valor de retorno. Verificar si este valor de error es `nil` te indicará si ocurrió un error.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("el valor debe ser 100 o menos")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Resultado:", result)
    }
    
    // Manejando un error grácilmente
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Error:", anotherErr)
    } else {
        fmt.Println("Resultado:", anotherResult)
    }
}
```

Salida de muestra para el código anterior:
```
Error: el valor debe ser 100 o menos
Resultado: 100
```

En este ejemplo, la función `Compute` devuelve un valor calculado o un error. El llamador maneja el error verificando si `err` no es `nil`.

## Estudio profundo

El enfoque de Go para el manejo de errores es deliberadamente sencillo y seguro en términos de tipos, requiriendo verificaciones explícitas de errores. Este concepto contrasta con el manejo de errores basado en excepciones visto en lenguajes como Java y Python, donde los errores se propagan hacia arriba en la pila de llamadas a menos que sean capturados por un manejador de excepciones. El equipo de Go argumenta que el manejo explícito de errores resulta en un código más claro y fiable, ya que obliga a los programadores a abordar los errores inmediatamente donde ocurren.

Sin embargo, algunas críticas mencionan que este patrón puede llevar a un código verboso, especialmente en funciones complejas con muchas operaciones propensas a errores. En respuesta, versiones más recientes de Go han introducido características de manejo de errores más sofisticadas, como el envoltorio de errores, lo que facilita proporcionar contexto a un error sin perder la información del error original. La comunidad también ha visto propuestas para nuevos mecanismos de manejo de errores, tales como check/handle, aunque estos permanecen en discusión a partir de mi última actualización.

La filosofía de manejo de errores de Go enfatiza la comprensión y la planificación de errores como parte del flujo normal del programa. Este enfoque fomenta el desarrollo de un software más resiliente y predecible, aunque con un posible aumento en el código de plantilla. Existen patrones y bibliotecas alternativas para agilizar el manejo de errores en casos particularmente complejos, pero el tipo `error` incorporado en Go sigue siendo la base del manejo de errores en el lenguaje.
