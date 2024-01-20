---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué? 

La conversión de una fecha a un string en programación es un proceso que cambia un objeto de fecha a un formato de texto. Los programadores realizan esto para hacer el dato más fácil de leer, manipular, almacenar o compartir.

## Cómo hacerlo:

Veamos un ejemplo de cómo convertir una fecha a un string en Go.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Now()
    fmt.Println(t.Format("2006-01-02"))
}
```

Al ejecutar este código, obtendrás una salida como esta:

```Go
2022-10-08
```

Aquí, `t.Format("2006-01-02")` se utiliza para convertir el objeto de tiempo `t` a una cadena de texto.

## Inmersión Profunda:

1. En términos de contexto histórico, el tiempo y las fechas han sido elementos esenciales para la computación desde los primeros días. Y dado que es común que los humanos lean y comprendan las fechas mejor en formato de texto que en un formato numérico, se ha convertido en una práctica estándar convertirlas a strings en la mayoría de los lenguajes de programación.

2. Hay diversas formas de hacer esto en Go además de `t.Format()`. Por ejemplo, también puedes usar `fmt.Sprintf()` que formatea y devuelve una cadena sin imprimir la salida.

```Go
str := fmt.Sprintf("Time : %s", time.Now().Format("2006-01-02"))
```

3. Internamente, el método `t.Format()` en Go utiliza un diseño de referencia específico 'Mon Jan 2 15:04:05 MST 2006' para formatear el tiempo. Esto es diferente de muchos lenguajes de programación que utilizan varios códigos de formato como '%Y' para el año, '%m' para el mes, etc.

## Ver También:

Para obtener más información, aquí tienes algunos recursos útiles:

- Documentación oficial de Go sobre el paquete `time`: https://pkg.go.dev/time
- Documentación oficial de Go sobre el paquete `fmt`: https://pkg.go.dev/fmt
- Un tutorial útil sobre el trabajo con fechas y horas en Go: https://yourbasic.org/golang/format-parse-string-time-date-example/