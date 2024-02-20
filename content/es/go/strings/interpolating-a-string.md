---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:22.687173-07:00
description: "La interpolaci\xF3n de cadenas es un m\xE9todo para construir cadenas\
  \ que incorporan variables, permitiendo la creaci\xF3n din\xE1mica de cadenas. Los\
  \ programadores\u2026"
lastmod: 2024-02-19 22:05:17.099110
model: gpt-4-0125-preview
summary: "La interpolaci\xF3n de cadenas es un m\xE9todo para construir cadenas que\
  \ incorporan variables, permitiendo la creaci\xF3n din\xE1mica de cadenas. Los programadores\u2026"
title: Interpolando una cadena de texto
---

{{< edit_this_page >}}

## Qué y por qué?

La interpolación de cadenas es un método para construir cadenas que incorporan variables, permitiendo la creación dinámica de cadenas. Los programadores hacen esto para personalizar mensajes, construir URLs, crear consultas SQL y más, permitiendo un código más legible y mantenible.

## Cómo hacerlo:

En Go, la interpolación de cadenas se logra comúnmente usando el paquete `fmt`, particularmente con la función `Sprintf`, que te permite inyectar variables en una cadena especificando verbos de formato. Los verbos son marcadores de posición en la cadena de formato y son reemplazados por los valores de las variables dadas. Así es como lo usas:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Usando Sprintf para la interpolación de cadenas
    message := fmt.Sprintf("Hola, mi nombre es %s y tengo %d años.", name, age)
    fmt.Println(message) // Salida: Hola, mi nombre es Jane y tengo 28 años.
}
```

Nota que `%s` se usa para cadenas, y `%d` para enteros. La documentación del paquete `fmt` proporciona una lista comprensiva de verbos de formato para diferentes tipos de datos.

## Profundización

El concepto de interpolación de cadenas existe en muchos lenguajes de programación, aunque con diferentes sintaxis y capacidades. En Go, mientras que la función `Sprintf` del paquete `fmt` es el enfoque más comúnmente usado, podría no ser siempre el más eficiente, especialmente para concatenaciones simples o cuando se trabaja dentro de código altamente sensible al rendimiento.

El paquete `fmt` utiliza reflexión para interpretar dinámicamente los tipos de las variables en tiempo de ejecución, lo que, aunque flexible, incurre en sobrecarga. Para escenarios donde el rendimiento es crítico, la concatenación directa de cadenas o el tipo `strings.Builder` pueden ofrecer alternativas mejores. La concatenación directa es sencilla pero puede volverse engorrosa con múltiples variables. `strings.Builder`, por otro lado, proporciona una forma más eficiente y legible de construir cadenas complejas en un bucle o al tratar con muchas variables:

```go
var sb strings.Builder
sb.WriteString("Hola, mi nombre es ")
sb.WriteString(name)
sb.WriteString(" y tengo ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" años.")
message := sb.String()

fmt.Println(message) // Produce la misma salida que antes
```

Finalmente, la elección entre `fmt.Sprintf`, concatenación directa y `strings.Builder` depende de los requisitos específicos de tu aplicación, como la complejidad de la cadena que se está construyendo y las consideraciones de rendimiento.
